all:
	ocamlfind ocamlc -g -o type -linkpkg -package mparser -linkpkg -package llvm type.ml

watch:
	ls *.ml | entr sh -c 'make && ./type'
