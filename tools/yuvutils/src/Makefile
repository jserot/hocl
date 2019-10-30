CC			= gcc
CCFLAGS     = -Wno-format -Wno-parentheses -Wno-unsequenced # gcc 4 compatibility
LDFLAGS	    =
SOURCES     = yuvutils.c
OBJS        = $(SOURCES:.c=.o)
TARGETLIB   = libyuvutils.a

all: libyuvutils.a yuv2ppm

install: libyuvutils.a yuv2ppm
	cp libyuvutils.a ../lib
	cp yuvutils.h ../include
	cp yuv2ppm ../bin

libyuvutils.a: $(OBJS)
	$(AR) -q $(TARGETLIB) $(OBJS)

yuv2ppm: yuv2ppm.c 
	$(CC) $(CFLAGS) -L. -o yuv2ppm $< -lyuvutils

.c.o:
	$(CC) $(CCFLAGS) -c -g -o $@ $<

clean:
	-rm -f *.o *~ *_old $(EXE) $(TARGETLIB)
	-rm -f core

clobber: realclean

realclean: clean
	-rm -f libyuvutils.a
	-rm -f yuv2ppm

yuvutils.o: yuvutils.c
