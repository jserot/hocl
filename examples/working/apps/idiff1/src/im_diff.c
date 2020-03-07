/*
	============================================================================
	Name        : im_diff.c
	Author      : mpelcat
    Modified by : jserot (for use in the HoCL framework)
	Version     : 1.2
	Copyright   : CECILL-C
	Description :
	============================================================================
*/

#include <string.h>
#include <stdlib.h>
#include <stdio.h>

#include "im_diff.h"

void im_diff(PARAM int width, PARAM int height,
             IN unsigned char *input, IN unsigned char *previous,
             OUT unsigned char *output, OUT unsigned char *result)
{
    int i,j;
    int d;
    // Copy input into output
    for(j=0; j<height; j++){
        for(i=0; i<width; i++){
            output[j*width + i] = input[j*width + i];
        }
    }
    // Set input-previous into result
    for(j=0; j<height; j++){
        for(i=0; i<width; i++){
          d = input[j*width + i]-previous[j*width + i];
          if ( d < 0 ) d = -d;
          result[j*width + i] = d; //*2;
          // result[j*width + i] = (input[j*width + i]-previous[j*width + i]);
        }
    }
}