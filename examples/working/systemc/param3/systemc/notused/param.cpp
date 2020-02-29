#include "param.h"

template<class T>
void Param<T>::main(void) {
    o.write(val);
    while ( 1 ) { 
      wait(clk.posedge_event());
    }
}
