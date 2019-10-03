include VERSION
include config

.PHONY: compiler tools clean doc install

all: compiler tools #doc

compiler:
#ifeq ($(BUILD_NATIVE),yes)
	dune build src/main.exe
# else
# 	dune build src/main.bc
# endif

tools:
# ifeq ($(BUILD_NATIVE),yes)
	dune build tools/pi2hcl/main.exe
# else
# 	dune build tools/pi2hcl/main.bc
# endif

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
	mkdir -p $(INSTALL_LIBDIR)
	cp -r lib/hocl $(INSTALL_LIBDIR)
	cp -r lib/etc $(INSTALL_LIBDIR)
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
ifeq ($(BUILD_DOC),yes)
	mkdir -p $(INSTALL_DOCDIR)
	cp doc/grammar.html $(INSTALL_DOCDIR)
	cp doc/tutorial.pdf $(INSTALL_DOCDIR)
endif

