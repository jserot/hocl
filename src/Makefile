all: byte

byte: 		
	dune build main.bc
	cp ../_build/default/src/main.bc ../bin/hoclc

native: 		
	dune build main.exe

.PHONY: doc

doc: 
	dune build grammar.tex grammar-defns.sty
#	dune build absyn.tex absyn-defns.sty
	dune build grammar.html
	sed 's/\\\\a0//g' grammar.html > ../doc/syntax.html

clean:
	dune clean
	\rm -f grammar.{html,tex}

clobber: clean 
	\rm -f *~
