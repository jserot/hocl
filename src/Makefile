OCB_FLAGS   = -use-ocamlfind -use-menhir -no-hygiene # -cflags '-unsafe-string -w -3-14-23-52'
OBELISK=obelisk
include ../config

OCB = 		ocamlbuild $(OCB_FLAGS)

all: 		byte # profile native debug

byte: 
			$(OCB) main.byte

native:
			$(OCB) main.native

tags:
	otags *.ml *.mli

install:
			$(CP) main.byte ../bin/hoclc

clean:
			$(OCB) -clean

clobber: clean 
			\rm -f *~

profile:
			$(OCB) -tag profile main.native

debug: 
			$(OCB) -tag debug main.byte

grammar.tex: parser.mly lexer.mll
	$(OBELISK) latex -i -prefix c2pi -o grammar.tex -package grammar-defns parser.mly

grammar.html: parser.mly lexer.mll
	$(OBELISK) html -i -o grammar.html parser.mly

.PHONY: all clean byte native profile debug tags doc options.tex grammar.tex
