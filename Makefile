include VERSION
include config

PACKNAME=rfsm

.PHONY: compiler clean doc install

all: compiler doc

compiler:
			(cd src; make)
ifeq ($(BUILD_NATIVE),yes)
			(cd src/compiler; make native)
endif

doc: 
	(cd src; make doc)
	pandoc -o CHANGELOG.html CHANGELOG.md
	pandoc -o README.html README.md

clean:
	(cd src; make clean)
	(cd lib; make clean)
	(cd examples; make clean)

clobber: 
	(cd src; make clobber)
	(cd lib; make clobber)
	(cd examples; make clobber)
	\rm -f *~

install:
	mkdir -p $(INSTALL_LIBDIR)
	cp -r lib/hocl $(INSTALL_LIBDIR)
	cp -r lib/etc $(INSTALL_LIBDIR)
	cp -r lib/systemc $(INSTALL_LIBDIR)
	mkdir -p $(INSTALL_BINDIR)
	cp src/main.byte $(INSTALL_BINDIR)/hocl
ifeq ($(BUILD_NATIVE),yes)
	cp src/main.opt $(INSTALL_BINDIR)/hocl.opt
endif
	mkdir -p $(INSTALL_DOCDIR)
	cp src/grammar.html $(INSTALL_DOCDIR)

