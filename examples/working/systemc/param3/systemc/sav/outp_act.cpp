// -------------------------------------------------------------------------------
// This file has been automatically generated by the HoCL compiler (version 1.0a)
// from file(s) main.hcl, on 2020-02-26 at 15:08:46, by jserot
// -------------------------------------------------------------------------------

#include "outp_act.h"
#include "./include/output.h"

void Outp::main(void) {
    outputInit();
    while ( 1 ) { 
      for ( int __k=0; __k<1; __k++ ) _i[__k] = i.read();
      output(_i);
    }
}
