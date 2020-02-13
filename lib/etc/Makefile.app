HOCL=/Users/jserot/Dev/ml/hocl
HOCLC=$(HOCL)/bin/hoclc
DOTVIEWER=graphviz
VCDVIEWER=gtkwave
TXTVIEWER=nano
PREESM_REP=/Users/jserot/Desktop/SF2/preesm-dev
PREESM_PROJ=/Users/jserot/Desktop/SF2/preesm-dev/$(PROJ)

all: check
#all: dot.show

check: $(SRCS)
	$(HOCLC) $(GEN_OPTS) $(SRCS)

dot: $(SRCS)
	$(HOCLC) $(GEN_OPTS) -dot $(DOT_OPTS) $(SRCS)

dot.show: dot
	@for f in *.dot; do $(DOTVIEWER) $$f; done

.PHONY: preesm systemc preesm.proj

preesm: $(SRCS)
	$(HOCLC) $(GEN_OPTS) -preesm $(PREESM_OPTS) $(SRCS)

preesm.proj: 
	if [ ! -d $(PREESM_PROJ) ]; then mkdir $(PREESM_PROJ); fi
	if [ ! -d $(PREESM_PROJ)/Algo ]; then mkdir $(PREESM_PROJ)/Algo; fi
	if [ ! -d $(PREESM_PROJ)/include ]; then mkdir $(PREESM_PROJ)/include; fi
	if [ ! -d $(PREESM_PROJ)/src ]; then mkdir $(PREESM_PROJ)/src; fi
	if [ ! -e $(PREESM_PROJ)/.project ]; then sed -e 's/%%NAME%%/$(PROJ)/' $(HOCL)/lib/preesm/.project.templ > $(PREESM_PROJ)/.project; fi
	cp include/*.h $(PREESM_PROJ)/include
	cp src/*.c $(PREESM_PROJ)/src
	cp preesm/*.pi $(PREESM_PROJ)/Algo

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
