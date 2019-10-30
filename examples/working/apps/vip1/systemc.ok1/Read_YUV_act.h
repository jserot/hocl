// -------------------------------------------------------------------------------
// This file has been automatically generated by the HoCL compiler (version 0.2)
// from file(s) main.hcl, on 2019-10-30 at 17:58:47, by jserot
// -------------------------------------------------------------------------------

#ifndef _Read_YUV_act_h
#define _Read_YUV_act_h

#include <systemc.h>
#include "hocl.h"

SC_MODULE(Read_YUV_act) {
  sc_fifo_in<int > width;
  sc_fifo_in<int > height;
  sc_fifo_out<uchar > y;
  sc_fifo_out<uchar > u;
  sc_fifo_out<uchar > v;

  void main(void);

  SC_HAS_PROCESS(Read_YUV_act);

  Read_YUV_act(sc_module_name name_, bool trace_=false  ) :
    modname(name_), sc_module(name_), trace(trace_) 
  {
    SC_THREAD(main);
  }

  ~Read_YUV_act() { }

  private:
    // Local variables
    int _width;
    int _height;
    uchar *_y;
    uchar *_u;
    uchar *_v;
    // Service
    bool trace;
    sc_module_name modname;
};
#endif