#ifndef _merge_h
#define _merge_h

#include <systemc.h>

template <class T>
SC_MODULE(Merge) { 
  sc_in<bool> clk;
  sc_fifo_in<bool> i1;
  sc_fifo_in<T> i2;
  sc_fifo_in<T> i3;
  sc_fifo_out<T> o;

  void main(void);

  SC_HAS_PROCESS(Merge);
  
  Merge(sc_module_name name_, bool trace_=false) :
    modname(name_), sc_module(name_), trace(trace_)
  {
    SC_THREAD(main);
  }

  ~Merge() { }

  private:
    sc_module_name modname;
    bool trace;
}; 

template <class T>
void Merge<T>::main(void) {
    while(1) { 
      wait(clk.posedge_event());
      if ( i1->read() ) 
        o->write(i2->read());
      else
        o->write(i3->read());
      }
}

#endif
