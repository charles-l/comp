open MParser

(*
sexp ::= '(' definition | atom {sexp | atom}* ')'
definition ::= define symbol : symbol
atom ::= symbol | string | number
symbol ::= letter { letter | number }*
number ::= digit+
string ::= '"' character* '"'
*)

type sexp =
    | Number of int
    | Nil
    | Pair of sexp * sexp
    | Symbol of string
    | String of string
    | Define of sexp * sexp

let str = char '"' >> many_chars (none_of "\"") << char '"' |>> (fun s -> String s)

(* don't have do notation, so this has to have a few inline lambda's *)
let sym = letter >>=
  (fun f -> many_chars (letter <|> digit) >>=
    fun r -> (return (Symbol (Char.escaped f ^ r))))

let number = many1_chars digit |>> fun n -> Number (int_of_string n)

let atom = sym <|> str <|> number

let rec pairs_from_list l = match l with
  | [] -> Nil
  | head :: tail -> Pair(head, pairs_from_list(tail))

let def = string "def" >> spaces >> sym >>=
  fun name ->
    (spaces >> char ':' >> spaces >> sym |>>
      fun ty -> Define (name, ty))

let rec sexp e = (atom <|> (between (char '(') (char ')') (def <|> ((sep_by sexp space) |>> pairs_from_list)))) e

let rec sexp_to_string e = match e with
                    | String s -> "\"" ^ s ^ "\""
                    | Symbol s -> "'" ^ s
                    | Number s -> string_of_int s
                    | Pair (a, b) -> "(" ^ (sexp_to_string a) ^ " . " ^ (sexp_to_string b) ^ ")"
                    | Nil -> "nil"
                    | Define (n, t) -> sexp_to_string n ^ " : " ^ sexp_to_string t

let parse (s: string) =
  match MParser.parse_string sexp s () with
    | Success e -> sexp_to_string e
    | Failed (msg, e) ->
        failwith msg ;;

let p e = print_string(e); print_newline();;

p(parse "\"asdf\"");
p(parse "\"\"");
p(parse "blah");
p(parse "31");
p(parse "(def x : int)");
