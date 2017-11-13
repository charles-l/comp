open Parse

let rec lookup env n =
  match env with
  | [] -> raise Not_found
  | h :: t ->
      try
        Hashtbl.find h n
      with _ ->
        lookup t n ;;

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
      | _ -> (iapply (ieval env (List.hd l)) (List.map (ieval env) (List.tl l))))
  | Primitive Symbol s ->
      lookup env s
  | Primitive l -> l
  | Definition _ -> Nil
  | Declaration _ -> Nil
and iapply f args =
  match f with
  | PLambda f ->
      (f args)
  | Lambda l ->
      let fenv = Hashtbl.create 32 in
      List.iter2 (fun a b -> Hashtbl.add fenv a b) l.params args;
      List.hd (List.rev (List.map (ieval (fenv :: l.env)) l.body))
  | _ -> raise (Error "cannot apply function") ;;

let make_default_env =
  let h = Hashtbl.create 32 in
  Hashtbl.add h "add1" (PLambda (fun x -> match x with
                        | [Number n] -> Number (n + 1)
                        | _ -> raise (Error "can't add to non-number")));
  Hashtbl.add h "fby" (PLambda (fun args ->
    let upf = (List.nth args 1) in
    (Stream {hd = List.hd args;
        cur = List.hd args;
        next = fun c -> (iapply upf [c])})));
  Hashtbl.add h "first" (PLambda (fun args ->
    (match (List.hd args) with
      | Stream s -> s.hd
      | _ -> raise (Error "failed"))));
  Hashtbl.add h "next" (PLambda (fun args ->
    (match (List.hd args) with
      | Stream s -> (Stream {hd = s.hd; cur = s.next s.cur; next = s.next})
      | _ -> raise (Error "failed"))));
  [h];;

print_endline (literal_to_string
  (ieval make_default_env
    (List.hd
      (parse "(next (next (fby 1 add1)))"))))
(*print_endline (literal_to_string (ieval (Hashtbl.create 25) (List.hd (parse "(quote 2)"))))*)
