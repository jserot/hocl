// -------------------------------------------------------------------------------
// This file has been automatically generated by the HoCL compiler (version 0.2)
// from file(s) main.hcl, on 2019-10-30 at 19:28:53, by jserot
// -------------------------------------------------------------------------------

#ifndef _index_param_h
#define _index_param_h

#include <systemc.h>
#include "hocl.h"

SC_MODULE(index_param) {
  sc_in<bool> clk;
  sc_fifo_out<int > o;

  void main(void);

  SC_HAS_PROCESS(index_param);

  index_param(sc_module_name name_ ) :
    modname(name_), sc_module(name_)  {
    SC_THREAD(main);
  }

  ~index_param() { }

  private:
    // Local variables
    int _o;
    // Service
    sc_module_name modname;
};
#endif
