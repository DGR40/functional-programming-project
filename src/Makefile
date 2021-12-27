MODULES=author node playerstate cards board play graph colors board2 run
OBJECTS=$(MODULES:=.cmo)
MLS=$(MODULES:=.ml)
MLIS=$(MODULES:=.mli)
TEST=test.byte
MAIN=run.byte
PLAYERSTATETEST=playerstate.byte
OCAMLBUILD=ocamlbuild -use-ocamlfind

default: build
	utop
	
docs: build
	mkdir -p _doc
	ocamlfind ocamldoc -I _build -package ANSITerminal,graphics \
		-html -stars -d _doc $(MLIS)
	
build:
	$(OCAMLBUILD) $(OBJECTS)

play:
	$(OCAMLBUILD) -tag 'debug' $(MAIN) && OCAMLRUNPARAM=b ./$(MAIN)

test:
	$(OCAMLBUILD) -tag 'debug' $(TEST) && ./$(TEST) -runner sequential

clean:
	ocamlbuild -clean
	rm -rf _doc finalproject.zip

zip:
	zip -r finalproject.zip *.ml* engine_test *.txt \
		_tags .merlin .ocamlformat .ocamlinit Makefile