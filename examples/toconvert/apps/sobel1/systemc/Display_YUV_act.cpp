// -------------------------------------------------------------------------------
// This file has been automatically generated by the HoCL compiler (version 0.2)
// from file(s) main.hcl, on 2019-11-05 at 15:07:51, by jserot
// -------------------------------------------------------------------------------

#include "Display_YUV_act.h"
#include "include/yuvDisplay.h"

void Display_YUV_act::main(void) {
    wait(clk.posedge_event());
    _id = id.read();
    _width = width.read();
    _height = height.read();
    yuvDisplayInit(_id, _width, _height);
    while ( 1 ) { 
      _id = id.read();
      _width = width.read();
      _height = height.read();
      _y = new uchar[_height*_width];
      _u = new uchar[((_height/2)*_width)/2];
      _v = new uchar[((_height/2)*_width)/2];
      for ( int __k=0; __k<_height*_width; __k++ ) _y[__k] = y.read();
      for ( int __k=0; __k<((_height/2)*_width)/2; __k++ ) _u[__k] = u.read();
      for ( int __k=0; __k<((_height/2)*_width)/2; __k++ ) _v[__k] = v.read();
      yuvDisplay(_id, _width, _height, _y, _u, _v);
      delete [] _y;
      delete [] _u;
      delete [] _v;
    }
}