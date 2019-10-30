CC=gcc
CFLAGS += -Wno-format -Wno-parentheses -Wno-unsequenced # gcc 4 compatibility
CSUPPLIBDIR=../src
CSUPPLIBNAME=yuv


all: test

.SUFFIXES:
.SUFFIXES: .c .h .o 

.c.o: 
	$(CC) $(CFLAGS) -I$(CSUPPLIBDIR) -c $<

test.o: test.c

test: test.o 
	$(CC) $(CFLAGS) -L$(CSUPPLIBDIR) -o test test.o -l$(CSUPPLIBNAME)

run: ./test
	./test
	echo $$?

clean:
	rm -f *.o *~ 
	rm -rf *.dSYM

realclean: clean
	rm -f ./test
	rm -f *.pgm *.ppm

clobber: realclean
