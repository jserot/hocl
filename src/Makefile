all: byte

byte: 		
	dune build main.bc
	cp ../_build/default/src/main.bc ../bin/hoclc

native: 		
	dune build main.exe

.PHONY: doc

doc: 
	dune build grammar.tex grammar-defns.sty
	dune build absyn.tex absyn-defns.sty

clean:
	dune clean
	\rm -f grammar.{html,tex}

clobber: clean 
	\rm -f *~
