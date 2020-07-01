(*
Language Specification

number: 23,12,32
boolean: #t,#f
*)

type given = {
  input:string;
  mutable  cur:int;
  max_len:int
}

type value =
  | Num of int
  | Bool of bool
;;

exception SyntaxError of string;;

let parse_num g =
  let rec parse_num_helper g n= 
    if g.cur<g.max_len
    then
      match g.input.[g.cur] with 
      | '-' | '0'..'9' -> 
        let c = Char.escaped g.input.[g.cur]
        in 
        g.cur<-g.cur+1;
        parse_num_helper g (n ^ c)
      | _ ->  
        n
    else
      n
  in
  let str = parse_num_helper g ""
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

let print_value a = 
  match a with
  | Num v -> print_string (string_of_int v)
  | Bool v -> if v=true then print_string "#t" else print_string "#f"
;;

let parse_sexpr g = 
  match g.input.[g.cur] with 
  | '-' | '0'..'9' -> print_value (parse_num g)
  | '#' -> print_value (parse_bool g)
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
