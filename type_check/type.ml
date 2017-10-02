open MParser

(* TODO: update this
sexp ::= '(' definition | atom {sexp | atom}* ')'
definition ::= 'def' symbol : {symbol | (symbol ...)}
atom ::= symbol | string | number
symbol ::= letter { letter | number }*
number ::= digit+
string ::= '"' character* '"'
*)

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

type ntpair = {name : string; ty : ltype}
type alist = {fname : string; args : literal list}

type ntype =
  | Definition of ntpair
  | Application of alist
  | Primitive of literal

exception Unbound_variable of string
exception Unknown_type of string
exception Wrong_type
exception Type_mismatch of ltype * ltype

(* parsing *)

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
                  | _ -> raise (Unknown_type t) in
    let ftype = (between (char '(') (char ')') (sep_by ident space)) |>>
      fun l -> FunctionT (List.map primtype l) in
      ftype <|> (ident |>> primtype)

let rec type_to_string t = match t with
                    | NumberT -> "number"
                    | BoolT -> "bool"
                    | StringT -> "string"
                    | FunctionT atypes -> "(" ^ List.fold_left (fun s e -> s ^ " " ^ (type_to_string e)) "->" atypes ^ ")"

let rec sexp e =
  let ntype_to_literal l = match l with
                            | Primitive l -> l
                            | _ -> raise Wrong_type
                            in
  let fcall = ident >>= fun f ->
    (spaces >> (sep_by sexp space) |>> fun l ->
      Application {fname=f; args=List.map ntype_to_literal l}) in
  let def = string "def" >> spaces >> ident >>= fun n ->
    (spaces >> char ':' >> spaces >> ptype |>> fun t ->
      Definition {name=n; ty=t}) in
  ((atom |>> fun l -> Primitive l) <|> (between (char '(') (char ')') (def <|> fcall))) e

let sexp_to_string s =
  let literal_to_string e = match e with
                    | String s -> "\"" ^ s ^ "\""
                    | True -> "t"
                    | Nil -> "nil"
                    | Symbol s -> "'" ^ s
                    | Number s -> string_of_int s in
  match s with
    | Definition d -> "(" ^ d.name ^ " : " ^ type_to_string d.ty ^ ")"
    | Application a -> "(" ^ a.fname ^ List.fold_left (fun s r -> s ^ " " ^ r) "" (List.map literal_to_string a.args) ^ ")"
    | Primitive l -> literal_to_string l

let parse (s: string) =
  match MParser.parse_string sexp s () with
    | Success e -> sexp_to_string e
    | Failed (msg, e) ->
        failwith msg ;;

let check_prototype proto app tbl =
  let arg_type p = match p with
      | String _ -> StringT
      | Number _ -> NumberT
      | True -> BoolT
      | Nil -> BoolT
      | Symbol s -> Hashtbl.find tbl s in
  let assert_same_type = fun pe ae -> if (pe != (arg_type ae)) then raise (Type_mismatch (pe, (arg_type ae))) in
  List.map2 assert_same_type proto app

let lookup_t tbl s = (try
  Hashtbl.find tbl s
    with Not_found -> raise (Unbound_variable s))


let annotate_tree e tbl =
  match e with
  | Application a -> check_prototype (match (lookup_t tbl a.fname) with
                                                | FunctionT l -> l
                                                | _ -> raise Wrong_type) a.args tbl; e
  | Definition n -> Hashtbl.add tbl n.name n.ty; e
  | Primitive _ -> e

let parse_and_annotate (s: string) =
  match MParser.parse_string sexp s () with
    | Success e -> annotate_tree e (Hashtbl.create 32)
    | Failed (msg, e) ->
        failwith msg ;;

let p e = print_string(e); print_newline();;

p(parse "(def x : number)");
p(parse "(def x : number)");
p(parse "(def add : (number number number)) (add 1 2)");
p(sexp_to_string (parse_and_annotate "((def add : (int int int)) (add 1 2))"));
p(sexp_to_string (parse_and_annotate "((def add : (int int int)) (add 1 t))"));
