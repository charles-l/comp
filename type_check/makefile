all:
	ocamlfind ocamlc -g -o type -linkpkg -package mparser -linkpkg -package llvm -linkpkg -package llvm.analysis -linkpkg type.ml

watch:
	ls *.ml | entr sh -c 'make && ./type'

debug:
	ocamldebug `ocamlfind query -recursive -i-format llvm` type
