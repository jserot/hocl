all: 
	dune build main.bc

clean:
	dune clean
	(cd examples; make clean)

clobber: clean
	\rm -f *~
	(cd examples; make clobber)

utop:
	dune utop .

