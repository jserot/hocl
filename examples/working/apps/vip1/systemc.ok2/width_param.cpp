// -------------------------------------------------------------------------------
// This file has been automatically generated by the HoCL compiler (version 0.2)
// from file(s) main.hcl, on 2019-10-30 at 17:58:47, by jserot
// -------------------------------------------------------------------------------

#include "width_param.h"

void width_param::main(void) {
    while ( 1 ) { 
      wait(clk.posedge_event());
      _o = 352;
      o.write(_o);
    }
}