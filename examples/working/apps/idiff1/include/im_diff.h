/*
	============================================================================
	Name        : difference.h
	Author      : mpelcat
    Modified by : jserot (for use in the HoCL framework)
	Version     : 1.1
	Copyright   : CeCILL-C
	Description : Difference between two images
	============================================================================
*/

#ifndef IM_DIFF_H
#define IM_DIFF_H

#include "hocl.h"

/**
* Function to compute the frame-to-frame difference 
*
* @param width
*        The width of the processed image
* @param width
*        The heigth of the processed image
* @param input
*        The input image of size height*width
* @param previous
*        The previous image of size height*width
* @param output
*        The copy of the input image of size height*width
* @param result
*        The difference image (input-previous) of size height*width
*/
void im_diff(PARAM int width, PARAM int height,
             IN unsigned char *input, IN unsigned char *previous,
             OUT unsigned char *output, OUT unsigned char *result);

#endif
