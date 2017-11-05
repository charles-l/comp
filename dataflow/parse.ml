open MParser
open Ppx_let

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
  | Stream of stream
  | True
  | Nil
and stream = {hd : literal; cur : literal; next : (literal -> literal)}

type ntype =
  | Declaration of binding
  | Definition of binding * ntype
  | Sexp of ntype list
  | Primitive of literal

exception Error of string
exception No_value

(*
ident ::= {symbol | letter} { symbol | letter | number }*
string ::= '"' character* '"'
symbol ::= ident
number ::= digit+
atom ::= symbol | string | number
sexp ::= '(' definition | atom {sexp | atom}* ')'
definition ::= 'def' symbol { '(' {'(' {symbol : symbol}+ ')'}* ')' }? : symbol sexp*
*)

let charsymbols = any_of "+-/*!%$?"

let ident =
  (letter <|> charsymbols) >>= fun f ->
    many_chars (alphanum <|> charsymbols) |>> fun r ->
      (Char.escaped f ^ r)

let str = char '"' >> many_chars (none_of "\"") << char '"' |>> (fun s -> String s)
let sym = ident |>> fun s -> match s with
                              | "t" -> True
                              | "nil" -> Nil
                              | _ -> Symbol s

let number =
  many1_chars digit |>> fun n ->
  Number (int_of_string n)
let atom = sym <|> str <|> number

let between_parens r = between (char '(') (char ')') r

let rec ptype e =
  let primtype = function
                  | "number" -> NumberT
                  | "bool" -> BoolT
                  | "string" -> StringT
                  | _ as t -> raise (Error ("unknown type " ^ t)) in
  let parse_binding = ((between_parens
  (ident >>= (fun na -> spaces >> ptype |>> fun ty -> {name=na; ty=ty}))) << spaces) in
  let parse_proto = (between_parens (many (not_followed_by ident "" >> parse_binding) >>= fun args -> (ptype |>> fun ret -> {args = args; retty = ret}))) in
  ((parse_proto |>> fun l -> FunctionT l) <|> (ident |>> primtype)) e

let rec type_to_string = function
                    | NumberT -> "number"
                    | BoolT -> "bool"
                    | StringT -> "string"
                    | FunctionT proto -> "(-> " ^ String.concat " " (List.map (fun b -> "(" ^ b.name ^ " " ^ type_to_string b.ty ^ ")") proto.args) ^ " " ^ (type_to_string proto.retty) ^ ")"

let rec sexp e =
  let fcall = ident >>= fun f ->
    (spaces >> (sep_by sexp space) |>> fun l -> Sexp ((Primitive (Symbol f)) :: l)) in
  let name_type_pair = ident >>= fun n -> spaces >> char ':' >> spaces >> ptype |>> fun t -> {name=n; ty=t} in
  let def = string "def" >> spaces >> ident >>= fun n ->
    spaces >> (option (between_parens (sep_by (between_parens name_type_pair) spaces))) >>=
    fun f ->
      spaces >> char ':' >> spaces >> ptype >>= fun ty ->
        let ty = match f with
        | None -> ty
        | Some v -> FunctionT {args=v; retty=ty} in
        spaces >>? (((*many*) sexp |>> fun body -> (Definition ({name=n; ty}, body))) <|> return (Declaration {name=n; ty}))
  in
  ((atom |>> fun l -> Primitive l) <|> (between_parens (def <|> fcall))) e

let sexp_list = sep_by sexp space

let rec sexp_to_string s =
  let rec literal_to_string = function
                    | String s -> "\"" ^ s ^ "\""
                    | True -> "t"
                    | Nil -> "nil"
                    | Symbol s -> "'" ^ s
                    | Stream _ -> "<...>"
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

let val_of_symbol = function
  | Symbol s -> s
  | _ -> raise (Error "Can't convert to symbol");;

let ntype_to_literal = function
  | Primitive l -> l
  | _ -> raise (Error "Can't convert to prim");;

let rec ieval env exp =
  match exp with
  | Sexp l -> (iapply (ntype_to_literal (List.hd l)) (List.map (ieval env) (List.tl l)))
  | Primitive Symbol s -> Hashtbl.find env s
  | Primitive l -> l
  | Definition _ -> Nil
  | Declaration _ -> Nil
and iapply f args =
  let apply_prim f args =
    match f with
  | "fby" ->
      let upf = (List.nth args 1) in
      Some (Stream {hd = List.hd args;
        cur = List.hd args;
        next = fun c -> (iapply upf [c])})
  | "first" ->
      (match (List.hd args) with
      | Stream s -> Some s.hd
      | _ -> raise (Error "failed"))
  | _ -> None in
  match apply_prim (val_of_symbol f) args with
  | None -> Nil
  | Some s -> s;;



parse "(def x : bool)";;
parse "(def x ((a : bool) (b : bool)) : number
         (+ 1 a))";;

let literal_to_string = function
  | Number i -> string_of_int i
  | Symbol s -> "'" ^ s
  | String s -> "\"" ^ s ^ "\""
  | Stream _ -> "<...>"
  | True -> "t"
  | Nil -> "nil" ;;

print_endline (literal_to_string (ieval (Hashtbl.create 25) (List.hd (parse "(first (fby 1 2))"))))
