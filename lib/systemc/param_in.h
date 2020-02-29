#ifndef _param_in_h
#define _param_in_h

#include <systemc.h>
#include "hocl.h"

template<class T>
SC_MODULE(param_in) {
  sc_in<bool> clk;
  sc_fifo_out<T> o;
  //sc_out<T> o;

  void main(void) {
    while ( 1 ) { 
      o.write(val);
      wait(clk.posedge_event());
      }
    }

  SC_HAS_PROCESS(param_in);

  param_in(sc_module_name name_, T val_, bool trace_=false  ) :
  modname(name_), sc_module(name_), val(val_), trace(trace_) 
  {
    SC_THREAD(main);
  }

  ~param_in() { }

  private:
    // Local variables
    T val;
    // Service
    bool trace;
    sc_module_name modname;
};
#endif
