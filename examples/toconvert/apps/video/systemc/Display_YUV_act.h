// -------------------------------------------------------------------------------
// This file has been automatically generated by the HoCL compiler (version 0.2)
// from file(s) main.hcl, on 2019-10-30 at 19:28:53, by jserot
// -------------------------------------------------------------------------------

#ifndef _Display_YUV_act_h
#define _Display_YUV_act_h

#include <systemc.h>
#include "hocl.h"

SC_MODULE(Display_YUV_act) {
  sc_in<bool> clk;
  sc_fifo_in<int > id;
  sc_fifo_in<int > width;
  sc_fifo_in<int > height;
  sc_fifo_in<uchar > y;
  sc_fifo_in<uchar > u;
  sc_fifo_in<uchar > v;

  void main(void);

  SC_HAS_PROCESS(Display_YUV_act);

  Display_YUV_act(sc_module_name name_, bool trace_=false  ) :
    modname(name_), sc_module(name_), trace(trace_) 
  {
    SC_THREAD(main);
  }

  ~Display_YUV_act() { }

  private:
    // Local variables
    int _id;
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
