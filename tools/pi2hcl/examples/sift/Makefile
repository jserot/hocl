PI2HCL=../../_build/default/main.bc

all: Hblur.hcl Hblur2x.hcl Hextract.hcl Htop_sift.hcl

% : %.pi
#	$(PI2HCL) -dot -names $@.names $<
	$(PI2HCL) -dot $<
	open -a Graphviz $@.dot

Hblur.hcl: Hblur.pi
Hblur2x.hcl: mini.pi
Hextract.hcl: Hextract.pi
Htop_sift.hcl: Htop_sift.pi

clean:
	\rm -f *.hcl
	\rm -f *.dot

clobber: clean
	\rm -f *~
