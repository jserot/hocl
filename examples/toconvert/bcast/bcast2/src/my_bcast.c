#include "my_bcast.h"

void bcast(int k, IN int *i, OUT int *o1, OUT int *o2)
{
  *o1 = *i*k;
  *o2 = *i*k;
}
