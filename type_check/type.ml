open Llvm
open MParser

type ltype =
  | NumberT
  | BoolT
  | StringT
  | FunctionT of ltype list

type literal =
  | Number of int
  | Symbol of string
  | String of string
  | True
  | Nil

type binding = {name : string; ty : ltype}
type proto = {name : string; args : binding list}

type ntype =
  | Definition of binding
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

let drop_last l = take ((List.length l) - 1) l


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

let rec ptype =
  let primtype t = match t with
                  | "number" -> NumberT
                  | "bool" -> BoolT
                  | "string" -> StringT
                  | _ -> raise (Error ("unknown type " ^ t)) in
    let ftype = (between (char '(') (char ')') (sep_by ident space)) |>>
      fun l -> FunctionT (List.map primtype l) in
      ftype <|> (ident |>> primtype)

let rec type_to_string t = match t with
                    | NumberT -> "number"
                    | BoolT -> "bool"
                    | StringT -> "string"
                    | FunctionT atypes -> "(-> " ^ String.concat " " (List.map type_to_string atypes) ^ ")"

let rec sexp e =
  let fcall = ident >>= fun f ->
    (spaces >> (sep_by sexp space) |>> fun l -> Sexp l) in
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
    | Sexp l -> "(" ^ List.fold_left (fun s r -> s ^ " " ^ r) "" (List.map sexp_to_string l) ^ ")"
    | Primitive l -> literal_to_string l

let parse (s: string) =
  match MParser.parse_string sexp_list s () with
    | Success e -> e
    | Failed (msg, e) ->
        failwith msg ;;

let lookup_t tbl s = (try
  match Hashtbl.find tbl s with
    | (ty, _) -> ty
  with Not_found -> raise (Error ("unbound variable " ^ s)))

let lookup_v tbl s = (try
  match Hashtbl.find tbl s with
    | (_, v) -> v
  with Not_found -> raise (Error ("unbound variable " ^ s)))

let check_prototype proto app tbl =
  let arg_type p = match p with
      | Primitive String _ -> StringT
      | Primitive Number _ -> NumberT
      | Primitive True -> BoolT
      | Primitive Nil -> BoolT
      | Primitive Symbol s -> lookup_t tbl s in
  let assert_same_type = fun pe ae -> if (pe != (arg_type ae)) then
    raise (Error ("expected " ^ (type_to_string pe) ^ " but got " ^ (type_to_string (arg_type ae)))) in
  try
    List.iter2 assert_same_type proto app
  with Invalid_argument _ -> raise (Error ("wrong number of arguments: " ^ (string_of_int (List.length app))))

let context = global_context ()
let the_module = create_module context "my comp"
let builder = builder context
let named_values:(string, (ltype * llvalue)) Hashtbl.t = Hashtbl.create 32

let rec lty_to_llvmty = function
  | NumberT -> i32_type context
  | BoolT -> i1_type context
  | StringT -> pointer_type (i8_type context)
  | FunctionT l -> function_type (lty_to_llvmty (last l)) (Array.of_list (List.map lty_to_llvmty (drop_last l)))

let rec emit tbl e =
  let emit_expr = function
    | True -> const_int (lty_to_llvmty BoolT) 1
    | Nil -> const_int (lty_to_llvmty BoolT) 0
    | Number v -> const_int (lty_to_llvmty NumberT) v
    | Symbol v | String v -> const_stringz context v in
  let defvar name ty = Hashtbl.add tbl name (ty, const_int (lty_to_llvmty NumberT) 0) in
  let emit_fundef name ty = declare_function name (lty_to_llvmty ty) the_module in
  let funsym = function
    | Primitive Symbol s -> s
    | _ -> raise (Error "need function symbol to apply") in
  let emit_app = function
    | Sexp [] -> raise (Error "empty function application")
    | Sexp (id :: args) -> check_prototype (match (lookup_t tbl (funsym id)) with (* drop the return value *)
                                                    | FunctionT l -> take ((List.length l) - 1) l
                                                    | _ -> raise (Error "wrong type")) args tbl in
  match e with
    | [] -> []
    | h :: t -> (match h with
      | Primitive p -> emit tbl t @ [emit_expr p]
      | Definition {name; ty = FunctionT _ as fty} -> emit tbl t @ [emit_fundef name fty]
      | Definition {name; ty = _ as vty} -> defvar name vty; emit tbl t
      | Sexp _ as a -> emit_app a; emit tbl t)

let p e = List.iter (fun s -> print_string s; print_newline()) e;;

(*p(List.map sexp_to_string (parse "(def x : number)"));*)

let t = parse "(def super : (bool string number))" in
let c = emit named_values t in
Llvm_analysis.assert_valid_module the_module;
List.iter dump_value c
