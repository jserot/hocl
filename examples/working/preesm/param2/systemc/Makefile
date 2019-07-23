SC_DIR		= /usr/local/systemc-2.3.1
SC_CXX      = g++
SC_LD	 	= g++
CXXFLAGS  += -I../include -Wno-deprecated -Wno-deprecated-writable-strings -Wno-parentheses-equality
SC_CXXFLAGS  += -I../../../../../lib/systemc -I../include -I$(SC_DIR)/include -Wno-deprecated -Wno-deprecated-writable-strings -Wno-parentheses-equality
LDFLAGS	    = -L$(SC_DIR)/lib-macosx64

CPP_SRCS = inp_act.cpp outp_act.cpp foo_act.cpp incrFactor_act.cpp multFactor_act.cpp main_top.cpp
C_SRCS = input.c output.c foo.c 
OBJS = $(C_SRCS:.c=.o) $(CPP_SRCS:.cpp=.o)
CPP_HDRS = $(CPP_SRCS:.cpp=.h)
GEN_SRCS = $(CPP_SRCS) $(CPP_HDRS)

all: run

%.o: %.cpp
	$(SC_CXX) $(SC_CXXFLAGS) -c $<

%.o: %.c
	$(SC_CXX) $(CXXFLAGS) -c $<

.PHONY: run exe clean clobber

exe: ./main_sc

./main_sc: $(OBJS)
	$(SC_LD) $(LDFLAGS) $(OBJS) -o main_sc -lsystemc  2>&1 | c++filt

run: ./main_sc 
	./main_sc

inp_act.o: inp_act.cpp
outp_act.o: outp_act.cpp
foo_act.o: foo_act.cpp
main_top.o: main_top.cpp
input.o: input.c
output.o: output.c
foo.o: foo.c

clean:
	\rm -f $(GEN_SRCS) 
	\rm -f *.o
	\rm -f main_sc

clobber: clean
	\rm -f *~
