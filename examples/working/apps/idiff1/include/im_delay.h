/*
	============================================================================
	Name        : im_delay.h
    Author      : jserot (for use in the HoCL framework)
	Version     : 1.1
	Description : Image delay
	============================================================================
*/

#ifndef IM_DELAY_H
#define IM_DELAY_H

#include "hocl.h"

// void im_delay_init(PARAM int width, PARAM int height, PARAM unsigned char ival);

void im_delay(PARAM int width, PARAM int height, PARAM unsigned char ival,
             IN unsigned char *input, OUT unsigned char *output);

#endif
