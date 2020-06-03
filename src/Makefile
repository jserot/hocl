all: byte

byte: 		
	dune build main.bc
	cp ../_build/default/src/main.bc ../bin/hoclc

native: 		
	dune build main.exe

.PHONY: doc

doc: 
	dune build grammar.tex grammar-defns.sty
#   Check dest dir before uncommenting the next line !
	cp grammar.tex grammar-defns.sty ../../doc/v1.2/syntax
	dune build grammar.html
	sed 's/\\\\a0//g' grammar.html > ../doc/syntax.html
#	dune build absyn.tex absyn-defns.sty

clean:
	dune clean
	\rm -f grammar.{html,tex}

clobber: clean 
	\rm -f *~
