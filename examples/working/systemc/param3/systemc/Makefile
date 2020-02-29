SC_DIR		= /usr/local/systemc-2.3.1
SC_CXX      = g++
SC_LD	 	= g++
CXXFLAGS  += -I./include -Wno-deprecated -Wno-deprecated-writable-strings -Wno-parentheses-equality
SC_CXXFLAGS  += -I../../../../../lib/systemc -I./include -I$(SC_DIR)/include -Wno-deprecated -Wno-deprecated-writable-strings -Wno-parentheses-equality
LDFLAGS	    = -L$(SC_DIR)/lib-macosx64

C_SRCS = input.c output.c mult.c 
CPP_SRCS = inp_act.cpp outp_act.cpp mult_act.cpp main.cpp
CPP_HDRS = inp_act.h outp_act.h mult_act.h inParam.h depParam.h top_gph.h
OBJS = $(C_SRCS:.c=.o) $(CPP_SRCS:.cpp=.o)
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
mult_act.o: mult_act.cpp
main.o: main.cpp
input.o: input.c
output.o: output.c
mult.o: mult.c

clean:
	\rm -f *.o
	\rm -f main_sc

clobber: clean
	\rm -f $(GEN_SRCS)
	\rm -f $(GEN_HDRS)
	\rm -f *~
