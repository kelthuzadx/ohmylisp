(*
Language Specification

number: 23,12,~32
boolean: #t,#f
symbol: hello,yang-yi,+,empty?
*)

type given = {
  input:string;
  mutable  cur:int;
  max_len:int
}

type value =
  | Num of int
  | Bool of bool
  | Symbol of string
;;

exception SyntaxError of string;;

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
      | _ ->  
        n
    else
      n
  in
  let str = helper g ""
  in
  Num (int_of_string str) 
;;

let parse_bool g= 
  g.cur <- g.cur+1;
  match g.input.[g.cur] with
  | 't' -> Bool true
  | 'f' -> Bool false
  | _ -> raise (SyntaxError "expect #t or #f of bool type\n")
;;

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
        | _ ->  
          Symbol n
      else
        Symbol n
    in
    helper g ""
  |_-> raise (SyntaxError "expect symbol here")
;;

let print_value a = 
  match a with
  | Num v -> print_string (string_of_int v)
  | Bool v -> if v=true then print_string "#t" else print_string "#f"
  | Symbol v -> print_string v
;;

let parse_sexpr g =
  match g.input.[g.cur] with 
  | '~' | '0'..'9' -> print_value (parse_num g)
  | '#' -> print_value (parse_bool g)
  |'*'|'/'|'>'|'<'|'='|'?'|'!'|'-'|'+'|'A'..'Z'|'a'..'z'-> 
    print_value (parse_symbol g) 
  | _ -> print_string "Error"
;;


let repl = 
  while true do
    print_string "> ";
    let input = read_line ()
    in
    let g = {input=input;cur=0;max_len=String.length input}
    in 
    parse_sexpr g;
    print_newline ();
    flush stdout;
  done
;;
