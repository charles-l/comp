SOURCES=parse.ml
PACKAGES=$(shell bash -c 'grep PKG .merlin | sed -s "s/^PKG//" | sed -s "s/ / -package /g"')
BIN=df

all:
	ocamlfind ocamlc -g -o $(BIN) $(SOURCES) -linkpkg $(PACKAGES)

watch:
	ls *.ml | entr sh -c "make && ./$(BIN)"

debug:
	ocamldebug `ocamlfind query -recursive -i-format $(PACKAGES)` $(BIN)
