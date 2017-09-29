open MParser

(*
sexp ::= '(' definition | atom {sexp | atom}* ')'
definition ::= 'def' symbol : symbol
atom ::= symbol | string | number
symbol ::= letter { letter | number }*
number ::= digit+
string ::= '"' character* '"'
*)

type ltype =
    | NumberT
    | BoolT
    | FunctionT of ltype list
    | UserT of string

type sexp =
    | Number of int
    | Nil
    | Pair of sexp * sexp
    | Symbol of string
    | String of string
    | Define of string * ltype
    | Function of sexp (* function has return value, arg types and body *)

let str = char '"' >> many_chars (none_of "\"") << char '"' |>> (fun s -> String s)

(* don't have do notation, so this has to have a few inline lambda's *)
let ident = letter >>=
  (fun f -> many_chars (letter <|> digit) |>>
    fun r -> Char.escaped f ^ r)

(* don't have do notation, so this has to have a few inline lambda's *)
let sym = ident |>> fun s -> Symbol s

let rec type_to_string t = match t with
                    | NumberT -> "number"
                    | BoolT -> "bool"
                    | FunctionT atypes -> "(" ^ List.fold_left (fun s e -> s ^ " " ^ (type_to_string e)) "->" atypes ^ ")"
                    | UserT t -> t

let number = many1_chars digit |>> fun n -> Number (int_of_string n)

let atom = sym <|> str <|> number

let rec pairs_from_list l = match l with
  | [] -> Nil
  | head :: tail -> Pair(head, pairs_from_list(tail))


let rec ptype =
  let primtype t = match t with
                  | "number" -> NumberT
                  | "bool" -> BoolT
                  | _ -> UserT t in
    let ftype = (between (char '(') (char ')') (sep_by ident space)) |>>
      fun l -> FunctionT (List.map primtype l) in
      ftype <|> (ident |>> primtype)

let def = string "def" >> spaces >> ident >>=
  fun name ->
    (spaces >> char ':' >> spaces >> ptype |>>
      fun ty -> Define (name, ty))

let rec sexp e = (atom <|> (between (char '(') (char ')') (def <|> ((sep_by sexp space) |>> pairs_from_list)))) e

let rec sexp_to_string e = match e with
                    | String s -> "\"" ^ s ^ "\""
                    | Symbol s -> "'" ^ s
                    | Number s -> string_of_int s
                    | Pair (a, b) -> "(" ^ (sexp_to_string a) ^ " . " ^ (sexp_to_string b) ^ ")"
                    | Nil -> "nil"
                    | Define (n, t) -> "[" ^ n ^ " : " ^ type_to_string t ^ "]"
                    | Function body -> "(lambda (...) ...)"

let parse (s: string) =
  match MParser.parse_string sexp s () with
    | Success e -> sexp_to_string e
    | Failed (msg, e) ->
        failwith msg ;;

type node =
  | Primitive of sexp
  | Variable of ltype
  | NPair of node * node

let rec annotate_tree e tbl =
  match e with
  | Symbol s -> Variable (Hashtbl.find tbl s)
  | Pair (a, b) -> NPair((annotate_tree a tbl), (annotate_tree b tbl))
  | Define (n, t) -> (Hashtbl.add tbl n t; (Primitive Nil))
  | _ -> Primitive e

let p e = print_string(e); print_newline();;

p(parse "((def x : int) (print x))");
p(parse "((def x : int) (print x))");
p(parse "((def f : (int int)) (print x))");
