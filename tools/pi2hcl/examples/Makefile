PI2HCL=../_build/default/main.bc

% : %.pi
	$(PI2HCL) -dot -names $@.names $<
	open -a Graphviz $@.dot

mini.hcl: mini.pi
sobel.hcl: sobel.pi
hier_top.hcl: hier_top.pi
hier_sub.hcl: hier_sub.pi
hier: hier_top hier_sub
bcast.hcl: bcast.pi

clean:
	\rm -f *.hcl
	\rm -f *.dot

clobber: clean
	\rm -f *~
