open Llvm
open Llvm_executionengine
open Llvm_target
open Llvm_scalar_opts
open MParser

type ltype =
  | NumberT
  | BoolT
  | StringT
  | FunctionT of proto
  | PairT of ltype * ltype
and binding = {name : string; ty : ltype}
and proto = {args : binding list; retty : ltype}

type literal =
  | Number of int
  | Symbol of string
  | String of string
  | Pair of literal * literal
  | True
  | Nil

type ntype =
  | Declaration of binding
  | Definition of binding * ntype
  | Sexp of ntype list
  | Primitive of literal

exception Error of string
exception No_value

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

let get = function
  | None -> raise No_value
  | Some v -> v

(* parsing *)

(*
ident ::= {symbol | letter} { symbol | letter | number }*
string ::= '"' character* '"'
symbol ::= ident
number ::= digit+
atom ::= symbol | string | number
sexp ::= '(' definition | atom {sexp | atom}* ')'
definition ::= 'def' symbol : {symbol | '(' symbol ... ')'}
*)

let charsymbols = any_of "+-/*!%$?"

let ident = (letter <|> charsymbols) >>=
  (fun f -> many_chars (alphanum <|> charsymbols) |>>
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

let rec type_to_string = function
                    | NumberT -> "number"
                    | BoolT -> "bool"
                    | StringT -> "string"
                    | PairT(a, b) -> "(" ^ (type_to_string a) ^ " " ^ (type_to_string b) ^")"
                    | FunctionT proto -> "(-> " ^ String.concat " " (List.map (fun b -> "(" ^ b.name ^ " " ^ type_to_string b.ty ^ ")") proto.args) ^ " " ^ (type_to_string proto.retty) ^ ")"

let rec sexp e =
  let fcall = ident >>= fun f ->
    (spaces >> (sep_by sexp space) |>> fun l -> Sexp ((Primitive (Symbol f)) :: l)) in
  let def = string "def" >> spaces >> ident >>= fun n ->
    (spaces >> char ':' >> spaces >> ptype >>= fun t ->
      spaces >>? (sexp |>> fun body -> Definition ({name=n; ty=t}, body)) <|> return (Declaration {name=n; ty=t})) in
  ((atom |>> fun l -> Primitive l) <|> (between (char '(') (char ')') (def <|> fcall))) e

let sexp_list = sep_by sexp space

let rec sexp_to_string s =
  let rec literal_to_string = function
                    | String s -> "\"" ^ s ^ "\""
                    | True -> "t"
                    | Nil -> "nil"
                    | Symbol s -> "'" ^ s
                    | Pair (a, b) -> "(" ^ (literal_to_string a) ^ " . " ^ (literal_to_string b) ^")"
                    | Number s -> string_of_int s in
  match s with
    | Declaration {name; ty} -> "(" ^ name ^ " : " ^ type_to_string ty ^ ")"
    | Definition (def, body) -> let s = sexp_to_string (Declaration def) in
                                String.sub s 0 ((String.length s) - 1) ^ " ...)"
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
let carp pp b = build_struct_gep pp 0 "carp" b ;;
let cdrp pp b = build_struct_gep pp 1 "cdrp" b ;;

let rec lty_to_llvmty = function
  | NumberT -> i32_type context
  | BoolT -> i1_type context
  | StringT -> pointer_type (i8_type context)
  | PairT (a, b) -> pointer_type (llpair a b)
  | FunctionT l -> function_type (lty_to_llvmty l.retty) (Array.of_list (List.map (fun b -> lty_to_llvmty b.ty) l.args))
  and llpair a b = (struct_type context [|lty_to_llvmty a; lty_to_llvmty b|])

let prims = Hashtbl.create 32;;

let build_cons car cdr lbl b =
  let pp = build_alloca (llpair NumberT NumberT) lbl b in
  let _ = build_store car (carp pp b) b in
  let _ = build_store cdr (cdrp pp b) b in
  pp ;;

let rec emit fpm =
  let emit_expr = function
    | True -> const_int (lty_to_llvmty BoolT) 1
    | Nil -> const_int (lty_to_llvmty BoolT) 0
    | Number v -> const_int (lty_to_llvmty NumberT) v
    | Symbol v -> (try (match Hashtbl.find named_values v with
                   | (t, v) -> v)
                  with e -> raise (Error ("couldn't find " ^ v)))
    | Pair (a, b) -> build_alloca (lty_to_llvmty (PairT (NumberT, NumberT))) "pair1" builder
    | String v -> const_stringz context v in
  let defvar name ty = Hashtbl.add named_values name (ty, const_int (lty_to_llvmty NumberT) 0) in
  let emit_fundef ?(body = None) name fp =
    Hashtbl.clear named_values;
    let f = declare_function name (lty_to_llvmty (FunctionT fp)) the_module in
    if body != None then
      (Array.iteri(fun i a ->
        let n = List.nth fp.args i in
        set_value_name n.name a;
        Hashtbl.add named_values n.name (n.ty, a)
      ) (params f);
      let bb = append_block context "entry" f in
      position_at_end bb builder;

      try
        let ret_val = (emit fpm) (get body) in
        let _ = build_ret ret_val builder in
        Llvm_analysis.assert_valid_function f;
        let _ = PassManager.run_function f fpm in
        f
      with e ->
        delete_function f;
        raise e) else f
      in
  let emit_app = function
    | [] -> raise (Error "empty function application")
    | (Primitive Symbol id :: args) ->
        if Hashtbl.mem prims id then
          ((Hashtbl.find prims id) args)
        else
          (let f = match lookup_function id the_module with
                                      | Some f -> f
                                      | None -> raise (Error ("unknown function " ^ id)) in
          let p = params f in
          let expect_n = Array.length p in
          let got_n = List.length args in
          if expect_n == got_n then () else
            raise (Error ("wrong number of args passed (expected " ^ (string_of_int expect_n) ^ " but got " ^ (string_of_int got_n) ^ ")"));
          let args = Array.of_list (List.map (emit fpm) args) in
          build_call f args "calltmp" builder)
    | _ -> raise (Error "cannot apply function")
        in
  function
    | Primitive p -> emit_expr p
    | Declaration {name; ty = FunctionT fp} -> emit_fundef name fp
    | Declaration {name; ty = _ as vty} -> defvar name vty; emit_expr Nil
    | Definition ({name; ty = FunctionT fp}, body) -> emit_fundef ~body:(Some body) name fp
    | Sexp a -> emit_app a
    | _ -> raise (Error "Failed to emit expression") ;;

let insert_primitives fpm =
  let emit = emit fpm in
  let emit_nth a n = (emit (List.nth a n)) in
  Hashtbl.add prims "+" (fun args -> build_add (emit_nth args 0) (emit_nth args 1) "tmpadd" builder);
  Hashtbl.add prims "-" (fun args -> build_sub (emit_nth args 0) (emit_nth args 1) "tmpsub" builder);
  Hashtbl.add prims "*" (fun args -> build_mul (emit_nth args 0) (emit_nth args 1) "tmpmul" builder);
  Hashtbl.add prims "/" (fun args -> build_udiv (emit_nth args 0) (emit_nth args 1) "tmpdiv" builder);
  Hashtbl.add prims "cons" (fun args -> build_cons (emit_nth args 0) (emit_nth args 1) "tmppair" builder);
  Hashtbl.add prims "car" (fun args -> build_load (carp (emit_nth args 0) builder) "tmpcar" builder);
  Hashtbl.add prims "cdr" (fun args -> build_load (cdrp (emit_nth args 0) builder) "tmpcdr" builder);
  Hashtbl.add prims "ieq?" (fun args -> build_icmp Icmp.Eq (emit_nth args 0) (emit_nth args 1) "tmpicmp" builder);
  Hashtbl.add prims "if" (fun args ->
                            let startbb = insertion_block builder in
                            let func = block_parent startbb in
                            let thenbb = append_block context "then" func in
                            position_at_end thenbb builder;
                            let thenval = emit_nth args 1 in
                            let newthenbb = insertion_block builder in
                            let elsebb = append_block context "else" func in
                            position_at_end elsebb builder;
                            let elseval = emit_nth args 2 in
                            let newelsebb = insertion_block builder in
                            let mergebb = append_block context "ifmerg" func in
                            position_at_end mergebb builder;
                            let incoming = [(thenval, newthenbb); (elseval, newelsebb)] in
                            let phi = build_phi incoming "iftmp" builder in
                            position_at_end startbb builder;
                            ignore (build_cond_br (emit_nth args 0) thenbb elsebb builder);
                            position_at_end newthenbb builder; ignore (build_br mergebb builder);
                            position_at_end newelsebb builder; ignore (build_br mergebb builder);
                            position_at_end mergebb builder;
                            phi);
  Hashtbl.add prims "begin" (fun args -> last (List.map emit args));;


initialize () ;; (* initialize the execution engine *)
let exec_engine = create the_module ;;
let fpm = PassManager.create_function the_module ;;

insert_primitives fpm;

ignore (PassManager.initialize fpm);;

let s = parse "(def fun : ((n number) number)
                 (if (ieq? n 2) 1 2))" ;;
let c = List.map (emit fpm) s;;

Llvm_analysis.assert_valid_module the_module;;
dump_module the_module;;

ignore (PassManager.run_function (List.hd c) fpm)

open Ctypes ;;
open Foreign ;;

let a = get_function_address "fun" (funptr (int @-> (returning int))) exec_engine ;;

print_int (a 2)
