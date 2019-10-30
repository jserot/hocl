#include "input.h"

static int cnt = 0;

void inputInit(void)
{
  cnt = 0;
}

void input(OUT int *o)
{
  for ( int i=0; i<NTOKS; i++ ) o[i]=cnt*10+i;
  cnt++;
}
