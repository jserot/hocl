/*
	============================================================================
	Name        : im_delay.c
    Author      : jserot (for use in the HoCL framework)
	Version     : 1.2
	Description :
	============================================================================
*/

#include <string.h>
#include <stdlib.h>
#include <stdio.h>

#include "im_delay.h"

void im_delay_init(PARAM int width, PARAM int height, PARAM unsigned char ival,
                   IN unsigned char *input, OUT unsigned char *output)
{
    int i,j;
    // Output initial tokens
    for ( j=0; j<height; j++ )
      for ( i=0; i<width; i++ )
        output[j*width+i] = ival;
}
void im_delay(PARAM int width, PARAM int height, PARAM unsigned char ival,
             IN unsigned char *input, OUT unsigned char *output)
{
    int i,j;
    // Simply copy input to output
    for ( j=0; j<height; j++ )
      for ( i=0; i<width; i++ )
        output[j*width+i] = input[j*width+i];
}
