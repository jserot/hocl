// -------------------------------------------------------------------------------
// This file has been automatically generated by the HoCL compiler (version 0.2)
// from file(s) main.hcl, on 2019-11-07 at 19:02:13, by jserot
// -------------------------------------------------------------------------------

#include "ival_param.h"

void ival_param::main(void) {
    while ( 1 ) { 
      wait(clk.posedge_event());
      _o = 0;
      o.write(_o);
    }
}
