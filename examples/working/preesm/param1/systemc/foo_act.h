// -------------------------------------------------------------------------------
// This file has been automatically generated by the HoCL compiler (version 0.1)
// from file(s) /Users/jserot/Dev/ml/hocl/lib/hocl/prelude.hcl,main.hcl, on 2019-05-18 at 10:11:10, by jserot
// -------------------------------------------------------------------------------

#ifndef _foo_act_h
#define _foo_act_h

#include <systemc.h>
#include "hocl.h"

SC_MODULE(foo_act) {
  sc_in<bool> clk;
  sc_in<int > k;
  sc_fifo_in<int > i;
  sc_fifo_out<int > o;

  void main(void);

  SC_HAS_PROCESS(foo_act);

  foo_act(sc_module_name name_, bool trace_=false  ) :
    modname(name_), sc_module(name_), trace(trace_) 
  {
    SC_THREAD(main);
    sensitive << clk.pos();
  }

  ~foo_act() { }

  private:
    // Local variables
    int _k;
    int _i;
    int _o;
    // Service
    bool trace;
    sc_module_name modname;
};
#endif
