all: byte

byte: 		
	dune build main.bc
	cp ../_build/default/src/main.bc ../bin/hoclc

native: 		
	dune build main.exe

clean:
	dune clean
	\rm -f grammar.{html,tex}

clobber: clean 
	\rm -f *~

doc: 
	dune build grammar.html
	dune build grammar.tex
	cp ../_build/default/src/grammar.html ../doc
	(cd ../doc; sed -i '' 's/\\\\/\\/g' grammar.html)
	cp ../_build/default/src/grammar.tex ../doc/lrm
	(cd ../doc/lrm; sed -i '' 's/\\\\/\\/g' grammar.tex)
#	cp ../_build/default/src/grammar-defns.sty ../doc/lrm
#   Do **NOT** copy [grammar-defns.sty] in [../doc/lrm]; we will be using a customized one
