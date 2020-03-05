#ifndef _delay_h
#define _delay_h

#include <systemc.h>
#include "hocl.h"

template<class T>
SC_MODULE(Delay) {
  sc_in<bool> clk;
  sc_fifo_in<T> iv;
  sc_fifo_in<T> i;
  sc_fifo_out<T> o;

  void main(void) {
    wait(clk.posedge_event());
    val = iv.read();  // Initial value
    o.write(val);
    if ( trace ) cout << modname << " put " << val << " at " << sc_time_stamp() << endl; 
    while ( 1 ) { 
      wait(clk.posedge_event());
      val = i.read();
      o.write(val);
      if ( trace ) cout << modname << " put " << val << " at " << sc_time_stamp() << endl; 
      }
    }

  SC_HAS_PROCESS(Delay);

 Delay(sc_module_name name_, bool trace_=false) :
  modname(name_), sc_module(name_), trace(trace_)
  {
    SC_THREAD(main);
  }

  ~Delay() { }

  private:
    // Local variables
    T val;
    // Service
    bool trace;
    sc_module_name modname;
};
#endif
