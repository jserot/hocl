PI2HCL=../_build/default/main.bc

all: mini

mini: mini.pi mini.names
	$(PI2HCL) -dot -names mini.names mini.pi
	open -a Graphviz mini.dot

sobel: sobel.pi 
	$(PI2HCL) -dot -names sobel.names sobel.pi
	open -a Graphviz sobel.dot

clean:
	\rm -f *.hcl
	\rm -f *.dot

clobber: clean
	\rm -f *~
