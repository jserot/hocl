include config

.PHONY: compiler tools clean doc install

all: compiler tools #doc

compiler:
	dune build src/main.exe
	dune build src/main.bc

tools:
	dune build tools/pi2hcl/main.exe
	dune build tools/pi2hcl/main.bc

doc: 
	(cd src; make doc)
	pandoc -o CHANGELOG.html CHANGELOG.md
	pandoc -o README.html README.md

clean:
	(cd tools/pi2hcl; make clean)
	(cd src; make clean)
	(cd lib; make clean)
	(cd examples; make clean)

clobber: 
	(cd tools/pi2hcl; make clobber)
	(cd src; make clobber)
	(cd lib; make clobber)
	(cd examples; make clobber)
	\rm -f *~

install:
	cp ./config ./lib/etc
	mkdir -p $(INSTALL_LIBDIR)
	cp -r lib/hocl $(INSTALL_LIBDIR)
	mkdir -p $(INSTALL_LIBDIR)/etc
	cat ./platform ./lib/etc/Makefile.templ > $(INSTALL_LIBDIR)/etc/Makefile.app
	cp $(INSTALL_LIBDIR)/etc/Makefile.app ./lib/etc
	cp -r lib/systemc $(INSTALL_LIBDIR)
	cp -r lib/preesm $(INSTALL_LIBDIR)
	mkdir -p $(INSTALL_BINDIR)
#ifeq ($(BUILD_NATIVE),yes)
	cp _build/default/src/main.exe $(INSTALL_BINDIR)/hoclc
	cp _build/default/tools/pi2hcl/main.exe $(INSTALL_BINDIR)/pi2hcl
# else
# 	cp _build/default/src/main.bc $(INSTALL_BINDIR)/hoclc
# 	cp _build/default/tools/pi2hcl/main.bc $(INSTALL_BINDIR)/pi2hcl
# endif
# ifeq ($(BUILD_DOC),yes)
# 	mkdir -p $(INSTALL_DOCDIR)
# 	cp doc/grammar.html $(INSTALL_DOCDIR)
# 	cp doc/tutorial.pdf $(INSTALL_DOCDIR)
# endif

