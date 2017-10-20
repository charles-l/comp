PACKAGES="mparser \
		 llvm \
		 llvm.analysis \
		 llvm.executionengine \
		 llvm.target \
		 llvm.scalar_opts"

all:
	ocamlfind ocamlc -g -o type type.ml -linkpkg -package $(PACKAGES)

watch:
	ls *.ml | entr sh -c 'make && ./type'

debug:
	ocamldebug `ocamlfind query -recursive -i-format llvm` type
