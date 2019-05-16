HOCL=/Users/jserot/Dev/ml/hocl/src/main.byte
HOCL_OPTS=-prelude /Users/jserot/Dev/ml/hocl/lib/hocl/prelude.hcl
DOTVIEWER=graphviz

all: dot.show

dot: $(SRCS)
	$(HOCL) $(HOCL_OPTS) -dot $(DOT_OPTS) $(SRCS)

dot.show: dot
	$(DOTVIEWER) $(MAIN).dot

.PHONY: preesm

preesm: $(SRCS)
	$(HOCL) $(HOCL_OPTS) -preesm $(PREESM_OPTS) $(SRCS)

clean:
	@\rm -f *.dot
	@\rm -f *.output

clobber: clean
	@\rm -f *~
