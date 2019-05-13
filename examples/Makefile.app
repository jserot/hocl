HOCL=/Users/jserot/Dev/ml/hocl/src/main.byte
DOTVIEWER=graphviz

all: dot.show

dot: $(SRCS)
	$(HOCL) -dot $(DOT_OPTS) $(SRCS)

dot.show: dot
	$(DOTVIEWER) $(MAIN).dot

clean:
	@\rm -f *.dot
	@\rm -f *.output

clobber: clean
	@\rm -f *~
