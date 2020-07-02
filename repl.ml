(*
Language Specification

number: 23,12,~32
boolean: #t,#f
symbol: hello,yang-yi,+,empty?
list: (), (1 2 3) (~3 #t 90)
*)

type given = {
  input:string;
  mutable  cur:int;
  max_len:int
}

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
        match g.input.[g.cur] with 
        | '~'->
          g.cur <- g.cur+1;
          helper g ( n^"-")
        | '0'..'9' -> 
          let c = Char.escaped g.input.[g.cur]
          in 
          g.cur<-g.cur+1;
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
    g.cur <- g.cur+1;
    match g.input.[g.cur] with
    | 't' -> Bool true
    | 'f' -> Bool false
    | _ -> raise (SyntaxError "expect #t or #f of bool type\n")
  in

  let parse_symbol g =
    match g.input.[g.cur] with 
    |'*'|'/'|'>'|'<'|'='|'?'|'!'|'-'|'+'|'A'..'Z'|'a'..'z'-> 
      let rec helper g n= 
        if g.cur<g.max_len
        then
          match g.input.[g.cur] with 
          |'*'|'/'|'>'|'<'|'='|'?'|'!'|'-'|'+'|'A'..'Z'|'a'..'z'-> 
            let c = Char.escaped g.input.[g.cur]
            in 
            g.cur<-g.cur+1;
            helper g (n ^ c)
          | _ -> Symbol n
        else
          Symbol n
      in
      helper g ""
    |_-> raise (SyntaxError "expect symbol here")
  in
  let rec consume_delimiter g = 
    match g.input.[g.cur] with
    |' '|'\t'|'\n'|'\r'-> g.cur<-g.cur+1;consume_delimiter g;
    | _-> ()
  in
  let rec parse_list g =
    match g.input.[g.cur] with
    | '(' -> g.cur<-g.cur+1;parse_list g
    | ')' -> g.cur<-g.cur+1;Nil
    | _->
      let fst = consume_delimiter g;parse_sexpr g
      and snd = consume_delimiter g;parse_list g
      in
      Pair (fst,snd)
  in
  match g.input.[g.cur] with 
  | '~' | '0'..'9' -> parse_num g
  | '#' -> parse_bool g
  | '(' -> parse_list g
  |'*'|'/'|'>'|'<'|'='|'?'|'!'|'-'|'+'|'A'..'Z'|'a'..'z'-> 
    parse_symbol g
  | _ -> raise (SyntaxError "Invalid input")
;;


let repl = 
  while true do
    print_string "> ";
    let input = read_line ()
    in
    let g = {input=input;cur=0;max_len=String.length input}
    in 
    let sexpr = parse_sexpr g
    in
    print_value sexpr;
    print_newline ();
    flush stdout
  done
;;
