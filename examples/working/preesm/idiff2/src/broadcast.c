#include "broadcast.h"
#include <string.h>

void bcast(int width, int height, IN uchar *i, OUT uchar *o1, OUT uchar *o2)
{
  // Nothing special here ...
  memcpy(o1, i, width*height); 
  memcpy(o2, i, width*height);
}
