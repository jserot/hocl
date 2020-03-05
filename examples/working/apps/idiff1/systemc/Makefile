SC_DIR		= /usr/local/systemc-2.3.1
SC_CXX      = g++
SC_LD	 	= g++
SDL2    = /Users/jserot/Library/Frameworks/SDL2.framework
SDL2_TTF =/Users/jserot/Library/Frameworks/SDL2_ttf.framework
CXXFLAGS  += \
	-DSYSTEMC_TARGET \
	-I../../../../../lib/systemc \
	-I$(SC_DIR)/include \
	-I$(SDL2)/include \
	-I$(SDL2_TTF)/include \
	-I../../../../../lib/systemc \
	-I$(SC_DIR)/include \
	-I../include \
	-Wno-deprecated -Wno-deprecated-writable-strings -Wno-parentheses-equality
SC_CXXFLAGS  += -I../../../../../lib/systemc -I./include -I$(SC_DIR)/include -Wno-deprecated -Wno-deprecated-writable-strings -Wno-parentheses-equality
LDFLAGS	    = -L$(SC_DIR)/lib-macosx64 -L$(SDL2) -L$(SDL2_TTF)

C_SRCS = yuvRead.c yuvDisplay.c im_diff.c im_delay.c
CPP_SRCS = ReadYUV_act.cpp DisplayYUV_act.cpp ImDiff_act.cpp ImDelay_act.cpp main.cpp
CPP_HDRS = ReadYUV_act.h DisplayYUV_act.h ImDiff_act.h ImDelay.act.h top_gph.h
OBJS = $(C_SRCS:.c=.o) $(CPP_SRCS:.cpp=.o)
GEN_SRCS = $(CPP_SRCS) $(CPP_HDRS)
GEN_DATA =

all: run

%.o: %.cpp
	$(SC_CXX) $(SC_CXXFLAGS) -c $<

%.o: %.c
	$(SC_CXX) $(CXXFLAGS) -c $<

.PHONY: run exe clean clobber

exe: ./main_sc

OBJS = $(C_SRCS:.c=.o) $(CPP_SRCS:.cpp=.o)

./main_sc: $(OBJS)
	$(SC_LD) $(LDFLAGS) $(OBJS) -o main_sc -lsystemc -lsdl2 -lsdl2_ttf 2>&1 | c++filt

run: ./main_sc 
	./main_sc

yuv.o: yuv.c
yuvRead.o: yuvRead.c
im_diff.o: im_diff.c
im_delay.o: im_delay.c
yuvDisplay.o: yuvDisplay.c
main.o: main.cpp
ReadYUV_act.o: ReadYUV_act.cpp
DisplayYUV_act.o: DisplayYUV_act.cpp
ImDiff_act.o: ImDiff_act.cpp
ImDelay_act.o: ImDelay_act.cpp

clean:
	\rm -f *.o
	\rm -f main_sc

clobber: clean
	\rm -f $(GEN_SRCS)
	\rm -f $(GEN_HDRS)
	\rm -f $(GEN_DATA)
	\rm -f *~
