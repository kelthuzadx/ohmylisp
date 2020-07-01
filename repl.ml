open String;;

type value =
  | Num of int
;;

type given = {input:string;  cur:int ;max_len:int}

exception SyntaxError of string;;

let repl = 
  while true do
    print_string "> ";
    let input = read_line ()
    in
    let g = {input=input;cur=0;max_len=String.length input}
    in 
    print_string g.input;
    print_newline ();
    flush stdout;
  done
;;
