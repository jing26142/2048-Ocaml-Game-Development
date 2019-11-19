MODULES=command grid interface state
OBJECTS=$(MODULES:=.cmo)
MLS=$(MODULES:=.ml)
MLIS=$(MODULES:=.mli)
TEST=test.byte
MAIN=interface.byte
OCAMLBUILD=ocamlbuild -use-ocamlfind

default: build 
	utop

build:
	$(OCAMLBUILD) $(OBJECTS)

test:
	$(OCAMLBUILD) -tag 'debug' $(TEST) && ./$(TEST)


play:
	$(OCAMLBUILD) $(MAIN) && ./$(MAIN)

zip:
	zip 2048.zip *.ml* _tags *.txt* Makefile .merlin .ocamlinit