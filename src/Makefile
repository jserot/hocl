all: byte

byte: 		
	dune build main.bc
	cp ../_build/default/src/main.bc ../bin/hoclc

native: 		
	dune build main.exe

clean:
	dune clean

clobber: clean 
	\rm -f *~

doc: 
	dune build grammar.html
	dune build grammar.tex
	cp grammar.html ../doc
