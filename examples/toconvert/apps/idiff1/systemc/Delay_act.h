// -------------------------------------------------------------------------------
// This file has been automatically generated by the HoCL compiler (version 0.2)
// from file(s) main.hcl, on 2019-11-07 at 19:02:13, by jserot
// -------------------------------------------------------------------------------

#ifndef _Delay_act_h
#define _Delay_act_h

#include <systemc.h>
#include "hocl.h"

SC_MODULE(Delay_act) {
  sc_in<bool> clk;
  sc_fifo_in<int > width;
  sc_fifo_in<int > height;
  sc_fifo_in<int > ival;
  sc_fifo_in<uchar > input;
  sc_fifo_out<uchar > output;

  void main(void);

  SC_HAS_PROCESS(Delay_act);

  Delay_act(sc_module_name name_, bool trace_=false  ) :
    modname(name_), sc_module(name_), trace(trace_) 
  {
    SC_THREAD(main);
  }

  ~Delay_act() { }

  private:
    // Local variables
    int _width;
    int _height;
    int _ival;
    uchar *_input;
    uchar *_output;
    // Service
    bool trace;
    sc_module_name modname;
};
#endif
