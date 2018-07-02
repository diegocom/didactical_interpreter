#!/bin/bash
rm -f giacoi syntax.cmi syntax.cmo functions.cmi functions.cmo main.cmi main.cmo semantics.cmi semantics.cmo lexer.cmi lexer.cmo lexer.ml parser.cmi parser.cmo parser.ml parser.mli
ocamlc -w -10 -c syntax.ml
ocamlc -w -10 -c functions.ml
ocamllex lexer.mll
ocamlyacc parser.mly    
ocamlc -w -10 -c parser.mli
ocamlc -w -10 -c lexer.ml
ocamlc -w -10 -c parser.ml
ocamlc -w -10 -c semantics.ml
ocamlc -w -10 -c main.ml
ocamlc -w -10 -o giacoi str.cma syntax.cmo functions.cmo lexer.cmo parser.cmo semantics.cmo main.cmo 
rm -f syntax.cmi syntax.cmo functions.cmi functions.cmo main.cmi main.cmo semantics.cmi semantics.cmo lexer.cmi lexer.cmo lexer.ml parser.cmi parser.cmo
exit
