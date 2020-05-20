all: byte

byte: 		
	dune build main.bc
	cp ../_build/default/src/main.bc ../bin/hoclc

native: 		
	dune build main.exe

.PHONY: doc

doc: 
	dune build grammar.tex grammar-defns.sty
#   Check dest dir before uncommenting the next two lines !
#	sed 's/\\\\/\\/g' grammar.tex > ../../doc/v1.2.1/syntax/grammar.tex
#	sed 's/\\\\/\\/g' grammar-defns.sty > ../../doc/v1.2.1/syntax/grammar-defns.sty
#	dune build absyn.tex absyn-defns.sty
	dune build grammar.html
	sed 's/\\\\a0//g' grammar.html > ../doc/syntax.html

clean:
	dune clean
	\rm -f grammar.{html,tex}

clobber: clean 
	\rm -f *~
