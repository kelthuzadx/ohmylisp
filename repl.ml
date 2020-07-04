(* Handle given input *)

type 'a given = {
  input:string;
  mutable cur:int;
  max_len:int;
}

let next_char g =
  match g.cur<g.max_len with
  | true->  g.cur<-g.cur+1
  | false-> ()
;;

let has_next_char g = g.cur<g.max_len;;

let current_char g = g.input.[g.cur];;

(* Parser implementation *)
type value =
  | Nil
  | Num of int
  | Bool of bool
  | Symbol of string
  | Pair of value * value
;;

exception SyntaxError of string;;

let rec print_value a = 
  let rec print_flatten_pair p = 
    match p with
    | Pair(a',Nil)-> print_value a';
    | Pair(a',b') -> 
      print_value a';
      print_string " ";
      print_flatten_pair b';
    | other -> print_value other;
  in
  match a with
  | Nil -> print_string "nil"
  | Num v -> print_string (string_of_int v)
  | Bool v -> 
    ( match v with
      | true->print_string "#t" 
      |false-> print_string "#f"
    )
  | Symbol v -> print_string v
  | Pair (_,_)->
    print_string "(";
    print_flatten_pair a;
    print_string ")";
;;

let rec parse g =
  let parse_num g =
    let rec helper g n= 
      if has_next_char g
      then
        match current_char g with 
        | '~'->
          next_char g;
          helper g ( n^"-")
        | '0'..'9' -> 
          let c = Char.escaped (current_char g)
          in 
          next_char g;
          helper g (n ^ c)
        | _ -> n
      else
        n
    in
    let str = helper g ""
    in
    Num (int_of_string str) 
  in

  let parse_bool g= 
    next_char g;
    let ret =
      match current_char g with
      | 't' -> Bool true
      | 'f' -> Bool false
      | _ -> raise (SyntaxError ("expect #t or #f of bool type but got `#"^Char.escaped (current_char g)^"`"))
    in 
    next_char g;
    ret
  in

  let parse_symbol g =
    match current_char g with 
    |'*'|'/'|'>'|'<'|'='|'?'|'!'|'-'|'+'|'A'..'Z'|'a'..'z'-> 
      let rec helper g n= 
        if has_next_char g
        then
          match current_char g with 
          |'*'|'/'|'>'|'<'|'='|'?'|'!'|'-'|'+'|'A'..'Z'|'a'..'z'-> 
            let c = Char.escaped (current_char g)
            in 
            next_char g;
            helper g (n ^ c)
          | _ -> Symbol n
        else
          Symbol n
      in
      helper g ""
    |_-> raise (SyntaxError "expect symbol here")
  in
  let rec consume_delimiter g = 
    match current_char g with
    |' '|'\t'|'\n'|'\r'-> next_char g;consume_delimiter g;
    | _-> ()
  in
  let rec parse_list g =
    match current_char g with
    | '(' -> next_char g;parse_list g
    | ')' -> next_char g;Nil
    | _->
      let fst = consume_delimiter g;parse g
      and snd = consume_delimiter g;parse_list g
      in
      Pair (fst,snd)
  in
  match current_char g with 
  | '~' | '0'..'9' -> parse_num g
  | '#' -> parse_bool g
  | '(' -> parse_list g
  |'*'|'/'|'>'|'<'|'='|'?'|'!'|'-'|'+'|'A'..'Z'|'a'..'z'-> 
    parse_symbol g
  | _ -> raise (SyntaxError ("invalid input `"^(Char.escaped (current_char g))^"`") )
;;

(* Evaluation *)
exception UnboundError of string;;
exception RuntimeError of string;;

let bind (e, k, v) = Pair(Pair(Symbol k, v), e);;

let rec lookup (e, k) =
  match e with
  | Pair(Pair(Symbol k', v), e') ->
    if k=k' then v else lookup (e',k)
  | _ -> raise (UnboundError "unbound error")
;;

let rec list_of_pairs p = 
  match p with
  | Pair(a,Nil)-> a::[]
  | Pair(a,b)-> a::(list_of_pairs b)
  | _ -> raise (RuntimeError "list_of_pairs only takes arugment of pair type")
;;

let rec eval ~env ~expr = 
  match expr with
  | Nil -> (env,Nil)
  | Num a-> (env,Num a)
  | Bool a-> (env, Bool a)
  | Symbol a->(
      match a with
      | "env" -> (env,env)
      | _ -> (env, lookup (env,a))
    ) 
  | Pair(_,_) -> (
      match list_of_pairs expr with
      | [Symbol "val";Symbol name;e]->
        let _,value = eval env e in
        let env' = bind (env, name, value) in
        (env',value)
      |_->(env,expr)
    )
;;

(* main Read-Eval-Print-Loop *)

let repl = 
  let env  = ref Nil 
  in
  while true do
    print_string "> ";
    let input = read_line ()
    in
    try 
      if String.length input <> 0
      then
        let g = {input=input;cur=0;max_len=String.length input}
        in 
        let expr = parse g
        in
        (*print_value expr;*)
        let env',v = eval ~env:!env ~expr:expr
        in
        env := env';
        print_value v;
        print_newline ();
        flush stdout
      else
        ()
    with
    | SyntaxError msg -> print_string "SyntaxError: ";print_endline msg
    | UnboundError msg -> print_string "UnboundError: ";print_endline msg
    | _ -> print_string "RuntimeError: unknown reason";print_newline()
  done
;;
