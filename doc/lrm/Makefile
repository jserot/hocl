#SRCS=abssyn.tex dynamic.tex introduction.tex overview.tex static.tex syntax.tex typing.tex using.tex
SRCS=hocl.tex introduction.tex overview.tex syntax.tex 
GENSRCS=options-dot.tex options-general.tex options-systemc.tex options-preesm.tex options-dif.tex options-xdf.tex
DOC=hocl-lrm

all: $(DOC).pdf

ifeq ($(PLATFORM), win32)
install:
else
install: $(DOC).pdf
endif
	$(INSTALL) -d $(DOCDIR)
	$(CP) $(DOC).pdf $(DOCDIR)

$(DOC).pdf: $(SRCS) $(GENSRCS)
	pdflatex $(DOC).tex > pdflatex.log
	pdflatex $(DOC).tex > pdflatex.log

options-dot.tex:
	(cd ../../compiler; make tex_doc)

clean:
	rm -f *.dvi *.log *.rel *.aux *.toc

.PHONY: options_tex man_tex

realclean: clobber

clobber: clean
	rm -f $(DOC).pdf *~ 
	rm -f $(GENSRCS) 
