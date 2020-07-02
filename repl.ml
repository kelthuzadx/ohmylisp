(*
Language Specification

number: 23,12,~32
boolean: #t,#f
symbol: hello,yang-yi,+,empty?
list: (), (1 2 3) (~3 #t 90)
*)

type given = {
  input:string;
  mutable cur:int;
  max_len:int
}

let consume g =
  if g.cur<g.max_len 
  then
    g.cur<-g.cur+1
  else
    ()
;;

let current_char g = g.input.[g.cur];;

type value =
  | Nil
  | Num of int
  | Bool of bool
  | Symbol of string
  | Pair of value * value
;;

exception SyntaxError of string;;

let rec print_value a = 
  match a with
  | Nil -> print_string "nil"
  | Num v -> print_string (string_of_int v)
  | Bool v -> 
    if v=true then print_string "#t" else print_string "#f"
  | Symbol v -> print_string v
  | Pair (a',b') ->
    print_string "(";print_value a';print_string " ";print_value b';print_string ")";
;;

let rec parse_sexpr g =
  let parse_num g =
    let rec helper g n= 
      if g.cur<g.max_len
      then
        match current_char g with 
        | '~'->
          consume g;
          helper g ( n^"-")
        | '0'..'9' -> 
          let c = Char.escaped (current_char g)
          in 
          consume g;
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
    consume g;
    match current_char g with
    | 't' -> Bool true
    | 'f' -> Bool false
    | _ -> raise (SyntaxError ("expect #t or #f of bool type but got `#"^Char.escaped (current_char g)^"`"))
  in

  let parse_symbol g =
    match current_char g with 
    |'*'|'/'|'>'|'<'|'='|'?'|'!'|'-'|'+'|'A'..'Z'|'a'..'z'-> 
      let rec helper g n= 
        if g.cur<g.max_len
        then
          match current_char g with 
          |'*'|'/'|'>'|'<'|'='|'?'|'!'|'-'|'+'|'A'..'Z'|'a'..'z'-> 
            let c = Char.escaped (current_char g)
            in 
            consume g;
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
    |' '|'\t'|'\n'|'\r'-> consume g;consume_delimiter g;
    | _-> ()
  in
  let rec parse_list g =
    match current_char g with
    | '(' -> consume g;parse_list g
    | ')' -> consume g;Nil
    | _->
      let fst = consume_delimiter g;parse_sexpr g
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


let repl = 
  while true do
    print_string "> ";
    let input = read_line ()
    in
    try 
      if String.length input <> 0
      then
        let g = {input=input;cur=0;max_len=String.length input}
        in 
        let sexpr = parse_sexpr g
        in
        print_value sexpr;
        print_newline ();
        flush stdout
      else
        ()
    with
    |SyntaxError msg -> print_string "SyntaxError: ";print_endline msg
    |_ -> print_string "RuntimeError: unknown reason";print_newline()
  done
;;
