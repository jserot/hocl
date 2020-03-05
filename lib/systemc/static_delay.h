#ifndef _static_delay_h
#define _static_delay_h

#include <systemc.h>
#include "hocl.h"

template<class T>
SC_MODULE(static_delay) {
  sc_in<bool> clk;
  sc_fifo_in<T> i;
  sc_fifo_out<T> o;

  void main(void) {
    o.write(val);
    while ( 1 ) { 
      wait(clk.posedge_event());
      val = i.read();
      o.write(val);
      if ( trace ) cout << modname << " put " << val << " at " << sc_time_stamp() << endl; 
      }
    }

  SC_HAS_PROCESS(static_delay);

 static_delay(sc_module_name name_, T val_, bool trace_=false) :
  modname(name_), sc_module(name_), val(val_), trace(trace_)
  {
    SC_THREAD(main);
  }

  ~static_delay() { }

  private:
    // Local variables
    T val;
    // Service
    bool trace;
    sc_module_name modname;
};
#endif
