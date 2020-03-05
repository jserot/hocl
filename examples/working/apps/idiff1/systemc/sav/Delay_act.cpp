// -------------------------------------------------------------------------------
// This file has been automatically generated by the HoCL compiler (version 0.2)
// from file(s) main.hcl, on 2019-11-07 at 19:02:13, by jserot
// -------------------------------------------------------------------------------

#include "Delay_act.h"

void Delay_act::main(void) {
    wait(clk.posedge_event());
    _width = width.read();
    _height = height.read();
    _ival = ival.read();
    for ( int __k=0; __k<_height*_width; __k++ ) output.write(_ival); // Initial tokens
    while ( 1 ) { 
      _width = width.read();
      _height = height.read();
      _ival = ival.read();
      _input = new uchar[_height*_width];
      for ( int __k=0; __k<_height*_width; __k++ ) _input[__k] = input.read();
      for ( int __k=0; __k<_height*_width; __k++ ) output.write(_input[__k]);
      delete [] _input;
    }
}
