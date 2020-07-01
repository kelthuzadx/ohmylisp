type value =
  | Num of int
;;

type given = {
  input:string;
  mutable  cur:int;
  max_len:int
}

exception SyntaxError of string;;

let rec parse_num g n= 
  if g.cur<g.max_len
  then
    match g.input.[g.cur] with 
    | '0'..'9' -> 
      let c = Char.escaped g.input.[g.cur]
      in 
      g.cur<-g.cur+1;
      parse_num g (n ^ c)
    | _ ->  
      n
  else
    n
;;

let parse_sexpr g = 
  match g.input.[g.cur] with 
  | '0'..'9' -> print_string (parse_num g "")
  | _ -> print_int 0
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
