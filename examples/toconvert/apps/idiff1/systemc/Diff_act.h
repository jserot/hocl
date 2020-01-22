// -------------------------------------------------------------------------------
// This file has been automatically generated by the HoCL compiler (version 0.2)
// from file(s) main.hcl, on 2019-11-07 at 19:02:13, by jserot
// -------------------------------------------------------------------------------

#ifndef _Diff_act_h
#define _Diff_act_h

#include <systemc.h>
#include "hocl.h"

SC_MODULE(Diff_act) {
  sc_in<bool> clk;
  sc_fifo_in<int > width;
  sc_fifo_in<int > height;
  sc_fifo_in<uchar > input;
  sc_fifo_in<uchar > previous;
  sc_fifo_out<uchar > output;
  sc_fifo_out<uchar > result;

  void main(void);

  SC_HAS_PROCESS(Diff_act);

  Diff_act(sc_module_name name_, bool trace_=false  ) :
    modname(name_), sc_module(name_), trace(trace_) 
  {
    SC_THREAD(main);
  }

  ~Diff_act() { }

  private:
    // Local variables
    int _width;
    int _height;
    uchar *_input;
    uchar *_previous;
    uchar *_output;
    uchar *_result;
    // Service
    bool trace;
    sc_module_name modname;
};
#endif