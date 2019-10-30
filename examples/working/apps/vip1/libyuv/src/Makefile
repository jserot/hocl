CC			= gcc
CCFLAGS     = -Wno-format -Wno-parentheses -Wno-unsequenced # gcc 4 compatibility
LDFLAGS	    =
SOURCES     = yuv.c
OBJS        = $(SOURCES:.c=.o)
TARGETLIB   = libyuv.a

all: libyuv.a

libyuv.a: $(OBJS)
	$(AR) -q $(TARGETLIB) $(OBJS)

.c.o:
	$(CC) $(CCFLAGS) -c -g -o $@ $<

install: libyuv.a
	$(INSTALL) -d $(CAPH_C_LIB)
	$(INSTALL) $(TARGETLIB) $(CAPH_C_LIB)
	$(INSTALL) yuv.h $(CAPH_C_LIB)

clean:
	-rm -f *.o *~ *_old $(EXE) $(TARGETLIB)
	-rm -f a.out
	-rm -f core

clobber: realclean

realclean: clean
	-rm -f libyuv.a

yuv.o: yuv.c
