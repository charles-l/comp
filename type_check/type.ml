open Llvm
open MParser

type ltype =
  | NumberT
  | BoolT
  | StringT
  | FunctionT of proto
and binding = {name : string; ty : ltype}
and proto = {args : binding list; retty : ltype}

type literal =
  | Number of int
  | Symbol of string
  | String of string
  | True
  | Nil

type ntype =
  | Declaration of binding
  | Definition of binding * llvalue
  | Sexp of ntype list
  | Primitive of literal

exception Error of string

(* util *)

let last l = List.nth l ((List.length l) - 1)

let rec drop n h =
  if n == 0 then h else
    match h with
    | [] -> []
    | h::t -> drop (n - 1) t

let rec take n h =
  if n == 0 then []
  else
    match h with
    | [] -> []
    | h::t -> h :: (take (n - 1) t)

(* parsing *)

(*
ident ::= letter { letter | number }*
string ::= '"' character* '"'
symbol ::= ident
number ::= digit+
atom ::= symbol | string | number
sexp ::= '(' definition | atom {sexp | atom}* ')'
definition ::= 'def' symbol : {symbol | '(' symbol ... ')'}
*)

let ident = letter >>=
  (fun f -> many_chars (letter <|> digit) |>>
    fun r -> Char.escaped f ^ r)

let str = char '"' >> many_chars (none_of "\"") << char '"' |>> (fun s -> String s)
let sym = ident |>> fun s -> match s with
                              | "t" -> True
                              | "nil" -> Nil
                              | _ -> Symbol s

let number = many1_chars digit |>> fun n -> Number (int_of_string n)
let atom = sym <|> str <|> number

let rec ptype e =
  let primtype = function
                  | "number" -> NumberT
                  | "bool" -> BoolT
                  | "string" -> StringT
                  | _ as t -> raise (Error ("unknown type " ^ t)) in
  let parse_binding = ((between (char '(') (char ')')
  (ident >>= (fun na -> spaces >> ptype |>> fun ty -> {name=na; ty=ty}))) << spaces) in
  let parse_proto = (between (char '(') (char ')') (many (not_followed_by ident "" >> parse_binding) >>= fun args -> (ptype |>> fun ret -> {args = args; retty = ret}))) in
  ((parse_proto |>> fun l -> FunctionT l) <|> (ident |>> primtype)) e

let rec type_to_string t = match t with
                    | NumberT -> "number"
                    | BoolT -> "bool"
                    | StringT -> "string"
                    | FunctionT proto -> "(-> " ^ String.concat " " (List.map (fun b -> "(" ^ b.name ^ " " ^ type_to_string b.ty ^ ")") proto.args) ^ " " ^ (type_to_string proto.retty) ^ ")"

let rec sexp e =
  let fcall = ident >>= fun f ->
    (spaces >> (sep_by sexp space) |>> fun l -> Sexp ((Primitive (Symbol f)) :: l)) in
  let def = string "def" >> spaces >> ident >>= fun n ->
    (spaces >> char ':' >> spaces >> ptype |>> fun t ->
      Definition {name=n; ty=t}) in
  ((atom |>> fun l -> Primitive l) <|> (between (char '(') (char ')') (def <|> fcall))) e

let sexp_list = sep_by sexp space

let rec sexp_to_string s =
  let literal_to_string e = match e with
                    | String s -> "\"" ^ s ^ "\""
                    | True -> "t"
                    | Nil -> "nil"
                    | Symbol s -> "'" ^ s
                    | Number s -> string_of_int s in
  match s with
    | Definition {name; ty} -> "(" ^ name ^ " : " ^ type_to_string ty ^ ")"
    | Sexp l -> "(" ^ (String.concat " " (List.map sexp_to_string l)) ^ ")"
    | Primitive l -> literal_to_string l

let parse (s: string) =
  match MParser.parse_string sexp_list s () with
    | Success e -> e
    | Failed (msg, e) ->
        failwith msg ;;

let context = global_context ()
let the_module = create_module context "my comp"
let builder = builder context
let named_values:(string, (ltype * llvalue)) Hashtbl.t = Hashtbl.create 32

let rec lty_to_llvmty = function
  | NumberT -> i32_type context
  | BoolT -> i1_type context
  | StringT -> pointer_type (i8_type context)
  | FunctionT l -> function_type (lty_to_llvmty l.retty) (Array.of_list (List.map (fun b -> lty_to_llvmty b.ty) l.args))

let rec emit e =
  let emit_expr = function
    | True -> const_int (lty_to_llvmty BoolT) 1
    | Nil -> const_int (lty_to_llvmty BoolT) 0
    | Number v -> const_int (lty_to_llvmty NumberT) v
    | Symbol v | String v -> const_stringz context v in
  let defvar name ty = Hashtbl.add named_values name (ty, const_int (lty_to_llvmty NumberT) 0) in
  let emit_fundef name fp =
    Hashtbl.clear named_values;
    let f = declare_function name (lty_to_llvmty (FunctionT fp)) the_module in
    Array.iteri(fun i a ->
      let n = List.nth fp.args i in
      set_value_name n.name a;
      Hashtbl.add named_values n.name (n.ty, a)
    ) (params f);
    let bb = append_block context "entry" f in
    position_at_end bb builder;

    try
      let ret_val = emit_expr True in
      let _ = build_ret ret_val builder in
      Llvm_analysis.assert_valid_function f;
      f
    with e ->
      delete_function f;
      raise e
    in
  let emit_app = function
    | Sexp [] -> raise (Error "empty function application")
    | Sexp (Primitive Symbol id :: args) ->
        let f = match lookup_function id the_module with
                                    | Some f -> f
                                    | None -> raise (Error ("unknown function " ^ id)) in
        let p = params f in
        let expect_n = Array.length p in
        let got_n = List.length args in
        if expect_n == got_n then () else
          raise (Error ("wrong number of args passed (expected " ^ (string_of_int expect_n) ^ " but got " ^ (string_of_int got_n) ^ ")"));
        let args = Array.of_list (emit args) in
        build_call f args "calltmp" builder
    | _ as e -> raise (Error ("cannot apply " ^ (sexp_to_string e)))
        in
  match e with
    | [] -> []
    | h :: t -> (match h with
      | Primitive p -> emit t @ [emit_expr p]
      | Definition {name; ty = FunctionT fp} -> emit t @ [emit_fundef name fp]
      | Definition {name; ty = _ as vty} -> defvar name vty; emit t
      | Sexp _ as a -> emit t @ [emit_app a])

let t = parse "(def fun : ((thebool bool) (aname string) bool)) (fun t \"a\"))" ;;
List.iter print_endline (List.map sexp_to_string t)
let c = emit t;;
Llvm_analysis.assert_valid_module the_module;
List.iter dump_value c
