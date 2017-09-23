open MParser

(*
sexp ::= '(' atom {sexp | atom}* ')'
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

let str = char '"' >> many_chars (none_of "\"") << char '"' |>> (fun s -> String s)

(* don't have do notation, so this has to have a few inline lambda's *)
let sym = letter >>=
  (fun f -> many_chars (letter <|> digit) >>= fun r ->
    (return (Symbol (Char.escaped f ^ r))))

let number = many1_chars digit |>> fun n -> Number (int_of_string n)

let atom = sym <|> str <|> number

let rec pairs_from_list l = match l with
  | [] -> Nil
  | head :: tail -> Pair(head, pairs_from_list(tail))

let rec sexp e = (atom <|> (between (char '(') (char ')') (sep_by sexp space) |>> fun s -> pairs_from_list s)) e

let rec sexp_to_string e = match e with
                    | String s -> "\"" ^ s ^ "\""
                    | Symbol s -> "'" ^ s
                    | Number s -> string_of_int s
                    | Pair (a, b) -> "(" ^ (sexp_to_string a) ^ " . " ^ (sexp_to_string b) ^ ")"
                    | Nil -> "nil"

let eval (s: string) =
  match MParser.parse_string sexp s () with
    | Success e -> sexp_to_string e
    | Failed (msg, e) ->
        failwith msg ;;

print_string (eval "\"asdf\"");
print_newline();
print_string (eval "\"\"");
print_newline();
print_string (eval "blah");
print_newline();
print_string (eval "31");
print_newline();
print_string (eval "(asdf \"asdf\" 31 32)");
print_newline();
print_string (eval "(asdf)");
print_newline();

(*eval "(a b c)"*)
