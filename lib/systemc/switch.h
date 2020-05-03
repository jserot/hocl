#ifndef _switch_h
#define _switch_h

#include <systemc.h>

template <class T>
SC_MODULE(Switch) { 
  sc_in<bool> clk;
  sc_fifo_in<bool> sel;
  sc_fifo_in<T> i;
  sc_fifo_out<T> o1;
  sc_fifo_out<T> o2;

  void main(void);

  SC_HAS_PROCESS(Switch);
  
  Switch(sc_module_name name_, bool trace_=false) :
    modname(name_), sc_module(name_), trace(trace_)
  {
    SC_THREAD(main);
  }

  ~Switch() { }

  private:
    sc_module_name modname;
    bool trace;
}; 

template <class T>
void Switch<T>::main(void) {
    while(1) { 
      wait(clk.posedge_event());
      if ( sel->read() ) 
        o1->write(i->read());
      else
        o2->write(i->read());
      }
}

#endif
