HOCL=/Users/jserot/Dev/ml/hocl
HOCLC=$(HOCL)/bin/hoclc
DOTVIEWER=graphviz
VCDVIEWER=gtkwave
TXTVIEWER=nano

all: check
#all: dot.show

check: $(SRCS)
	$(HOCLC) $(GEN_OPTS) $(SRCS)

dot: $(SRCS)
	$(HOCLC) $(GEN_OPTS) -dot $(DOT_OPTS) $(SRCS)

dot.show: dot
	@if [ -d ./dot ]; then \
		for f in ./dot/*.dot; do $(DOTVIEWER) $$f; done; \
	else \
		for f in .*.dot; do $(DOTVIEWER) $$f; done; \
	fi 

.PHONY: dot preesm systemc preesm.proj

preesm: $(SRCS)
	$(HOCLC) $(GEN_OPTS) -preesm $(PREESM_OPTS) $(SRCS)

preesm.proj: 
	if [ ! -d $(PREESM_REP) ]; then mkdir $(PREESM_REP); fi
	if [ ! -d $(PREESM_REP)/Algo ]; then mkdir $(PREESM_REP)/Algo; fi
	if [ ! -d $(PREESM_REP)/include ]; then mkdir $(PREESM_REP)/include; fi
	if [ ! -d $(PREESM_REP)/src ]; then mkdir $(PREESM_REP)/src; fi
	if [ ! -e $(PREESM_REP)/.project ]; then sed -e 's/%%NAME%%/$(PREESM_PROJ)/' $(HOCL)/lib/preesm/.project.templ > $(PREESM_REP)/.project; fi
	cp include/*.h $(PREESM_REP)/include
	cp src/*.c $(PREESM_REP)/src
	cp preesm/*.pi $(PREESM_REP)/Algo

systemc: $(SRCS)
	$(HOCLC) $(GEN_OPTS) -systemc $(SYSTEMC_OPTS) $(SRCS)

xdf: $(SRCS)
	$(HOCLC) $(GEN_OPTS) -xdf $(XDF_OPTS) $(SRCS)

clean:
	@\rm -f *.dot
	@\rm -f *.xdf
	@\rm -f *.output
	@if [ -d ./systemc ]; then (cd ./systemc; make clean); fi
	@if [ -d ./preesm ]; then (cd ./preesm; rm -f *.pi); fi

clobber: clean
	@\rm -f *~
	@if [ -d ./systemc ]; then (cd ./systemc; make clobber); fi
	@if [ -d ./src ]; then (cd ./src; rm *~); fi
