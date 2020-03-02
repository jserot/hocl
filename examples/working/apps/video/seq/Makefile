CC      = gcc
LD	 	= gcc
SDL2    = /Users/jserot/Library/Frameworks/SDL2.framework
HOCL	= /Users/jserot/Dev/ml/hocl
SDL2_TTF =/Users/jserot/Library/Frameworks/SDL2_ttf.framework
CFLAGS  += -I$(SDL2)/include -I$(SDL2_TTF)/include -I../include -I$(HOCL)/lib/systemc -Wno-deprecated -Wno-deprecated-writable-strings -Wno-parentheses-equality
LDFLAGS	 = -L$(SDL2) -L$(SDL2_TTF)

C_SRCS = \
    yuvRead.c \
    yuvDisplay.c \
    test.c \

OBJS = $(C_SRCS:.c=.o) $(CPP_SRCS:.cpp=.o)

all: run

%.o: %.c
	$(CC) $(CFLAGS) -c $<

.PHONY: run exe clean clobber

exe: ./test

./test: $(OBJS)
	$(LD) $(LDFLAGS) $(OBJS) -o test 2>&1 -lsdl2 -lsdl2_ttf | c++filt

run: ./test
	./test

test.o: test.c
yuvRead.o: yuvRead.c
yuvDisplay.o: yuvDisplay.c

clean:
	\rm -f *.o
	\rm -f test

clobber: clean
	\rm -f *~
