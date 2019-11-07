#ifndef _MY_BCAST_H
#define _MY_BCAST_H

#include "defs.h"

// A special "bcast" fn which replicates its input [i] on both outputs [o1] and [o2]
// after having multiplied it by [k]

void bcast(int k, IN int *i, OUT int *o1, OUT int *o2);

#endif
