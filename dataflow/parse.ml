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
  | Stream of stream
  | Lambda of lambda
  | True
  | Nil
and stream = {hd : literal; cur : literal; next : (literal -> literal)}
and environment = (string, literal) Hashtbl.t list
and lambda = {params : string list; body : ntype list; env : environment}
and ntype =
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
  let parse_proto = (between_parens (many (not_followed_by ident "" >> parse_binding) >>=
    fun args -> (ptype |>> fun ret -> {args = args; retty = ret}))) in
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

let rec literal_to_string = function
  | Number i -> string_of_int i
  | Symbol s -> "'" ^ s
  | String s -> "\"" ^ s ^ "\""
  | Stream s -> (literal_to_string s.hd)
  | True -> "t"
  | Lambda _ -> "<lambda>"
  | Nil -> "nil" ;;

let rec sexp_to_string s =
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
  | _ as u -> raise (Error ("Can't convert " ^ literal_to_string u  ^ " to symbol"));;

let ntype_to_literal = function
  | Primitive l -> l
  | _ -> raise (Error "Can't convert to prim");;

let rec ieval env exp =
  match exp with
  | Sexp l ->
      (match l with
      | Primitive Symbol "quote" :: Primitive p :: _ -> p (* TODO: return a stream instead *)
      | Primitive Symbol "lambda" :: Sexp params :: body ->
          Lambda {params = (List.map (fun x -> x |> ntype_to_literal |> val_of_symbol) params); body; env}
      | _ -> (iapply (ntype_to_literal (List.hd l)) (List.map (ieval env) (List.tl l))))
  | Primitive Symbol s ->
      (* lookup primitive here *)
      Hashtbl.find (List.hd env) s
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
  | "next" ->
      (match (List.hd args) with
      | Stream s -> Some (Stream {hd = s.hd; cur = s.next s.cur; next = s.next})
      | _ -> raise (Error "failed"))
  | "add1" ->
      (match (List.hd args) with
      | Number n -> Some (Number (n + 1))
      | _ -> raise (Error "failed"))
  | _ -> None in
  match f with
  | Symbol s ->
      (match apply_prim s args with
    | None -> Nil
    | Some s -> s)
  | Lambda l ->
      let fenv = Hashtbl.create 32 in
      List.iter2 (fun a b -> Hashtbl.add fenv a b) l.params args;
      List.hd (List.rev (List.map (ieval (fenv :: l.env)) l.body))
  | _ -> raise (Error "cannot apply function") ;;


parse "(def x : bool)";;
parse "(def x ((a : bool) (b : bool)) : number
         (+ 1 a))";;

print_endline (literal_to_string (ieval [Hashtbl.create 25] (List.hd (parse "(next (fby 1 add1))"))))
(*print_endline (literal_to_string (ieval (Hashtbl.create 25) (List.hd (parse "(quote 2)"))))*)
