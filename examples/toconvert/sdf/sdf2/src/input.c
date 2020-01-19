#include "input.h"

static int cnt = 0;

void inputInit(int k)
{
  cnt = 0;
}

void input(int k, OUT int *o)
{
  for ( int i=0; i<k; i++ ) o[i]=cnt*10+i;
  cnt++;
}
