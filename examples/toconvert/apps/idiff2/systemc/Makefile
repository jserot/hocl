SC_DIR		= /usr/local/systemc-2.3.1
CC = g++
LD = g++
SDL2    = /Users/jserot/Library/Frameworks/SDL2.framework
SDL2_TTF =/Users/jserot/Library/Frameworks/SDL2_ttf.framework
CFLAGS  += \
	-DSYSTEMC_TARGET \
	-I../../../../../lib/systemc \
	-I$(SC_DIR)/include \
	-I$(SDL2)/include \
	-I$(SDL2_TTF)/include \
	-I../../../../../lib/systemc \
	-I$(SC_DIR)/include \
	-I../include \
	-Wno-deprecated -Wno-deprecated-writable-strings -Wno-parentheses-equality
LDFLAGS	    = -L$(SC_DIR)/lib-macosx64 -L$(SDL2) -L$(SDL2_TTF)

C_SRCS = \
    yuvRead.c \
    yuvDisplay.c \
	difference.c \
	broadcast.c
CPP_SRCS = \
	Read_YUV_act.cpp \
    Display_YUV_act.cpp \
    Diff_act.cpp \
    Delay_act.cpp \
    Bc1_act.cpp \
    height_param.cpp \
    width_param.cpp \
    ival_param.cpp \
    index_param.cpp \
    main_top.cpp
SRCS = $(C_SRCS) $(CPP_SRCS)

OBJS = $(C_SRCS:.c=.o) $(CPP_SRCS:.cpp=.o)
CPP_HDRS = $(CPP_SRCS:.cpp=.h)
GEN_SRCS = $(CPP_SRCS) $(CPP_HDRS)

all: run

%.o: %.cpp
	$(CC) $(CFLAGS) -c $<

%.o: %.c
	$(CC) $(CFLAGS) -c $<

.PHONY: run exe clean clobber

exe: ./main_sc

./main_sc: $(OBJS)
	$(LD) $(LDFLAGS) $(OBJS) -o main_sc -lsystemc -lsdl2 -lsdl2_ttf 2>&1 | c++filt

run: ./main_sc 
	./main_sc

yuvRead.o: yuvRead.c
yuvDisplay.o: yuvDisplay.c
difference.o: difference.c
broadcast.o: broadcast.c
Read_YUV_act.o: Read_YUV_act.cpp
Delay_act.o: Delay_act.cpp
Bc1_act.o: Bc1_act.cpp
height_param.o: height_param.cpp
width_param.o: width_param.cpp
ival_param.o: ival_param.cpp
index_param.o: index_param.cpp
main_top.o: main_top.cpp

clean:
	\rm -f *.o
	\rm -f main_sc

clobber: clean
	\rm -f $(GEN_SRCS)
	\rm -f *~
