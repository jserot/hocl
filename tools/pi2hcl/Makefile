all: misc.ml ir.ml dot.ml main.ml
	dune build main.bc

clean:
	dune clean

run:
	(cd examples; ../_build/default/main.bc -names mini.names mini.pi)
#	dune exec ./main.bc examples/sobel.pi

tests:
	@for f in examples/*.pi; do (dune exec ./main.bc $$f; open -a Graphviz $${f/pi/dot}); done

utop:
	dune utop .

