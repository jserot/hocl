#ifndef _depParam_h
#define _depParam_h

#include <systemc.h>
#include "hocl.h"

template<class T>
SC_MODULE(DepParam) {
  //sc_in<bool> clk;
  sc_in<T> i;
  sc_out<T> o;

  void main(void) {
    ival = i.read();
    oval = ival+1; // TO BE ADJUSTED
    o.write(oval);
    }

  SC_HAS_PROCESS(DepParam);

  DepParam(sc_module_name name_, bool trace_=false  ) :
  modname(name_), sc_module(name_), trace(trace_) 
  {
    SC_METHOD(main);
    sensitive << i;
  }

  ~DepParam() { }

  private:
    // Local variables
    T ival;
    T oval;
    // Service
    bool trace;
    sc_module_name modname;
};
#endif
