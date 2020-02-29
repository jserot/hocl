// -------------------------------------------------------------------------------
// This file has been automatically generated by the HoCL compiler (version 1.0a)
// from file(s) main.hcl, on 2020-02-26 at 15:08:46, by jserot
// -------------------------------------------------------------------------------

#ifndef _outp_act_h
#define _outp_act_h

#include <systemc.h>
#include "hocl.h"

SC_MODULE(Outp) {
  sc_in<bool> clk;
  sc_fifo_in<int > i;

  void main(void);

  SC_HAS_PROCESS(Outp);

  Outp(sc_module_name name_, bool trace_=false  ) :
    modname(name_), sc_module(name_), trace(trace_) 
  {
    SC_THREAD(main);
  }

  ~Outp() { }

  private:
    // Local variables
    int _i[1];
    // Service
    bool trace;
    sc_module_name modname;
};
#endif
