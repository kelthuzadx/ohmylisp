# Oh My Lisp
To learn ocaml and to write a subset of mit-scheme dialect


## getting started
```bash
 apple@mbp ~/Desktop/ohmylisp ocaml repl.ml     
> 23
23
> 12
12
> ~14
-14
> #t
#t
> #f
#f
> (3  63 3)
(3 63 3)
> (val t (3 5))
(3 5)
> (val q 2)
(2)
> env
((q 2) (t 3 5))
> (val q 11111111)
(11111111)
> env
((q 11111111) (q 2) (t 3 5))
> (val hello 12
RuntimeError: unknown reason
> (val hello 12)
(12)
>  
```

# plan
```scheme
null?
eq?
cons
car
cdr
not
and
or
```