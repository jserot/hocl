/*
	============================================================================
	Name        : sobel.h
	Author      : kdesnos
    Modified by : jserot (for use in the HoCL framework)
	Version     : 1.1
	Copyright   : CeCILL-C
	Description : 2D Sobel filtering function
	============================================================================
*/

#ifndef SOBEL_H
#define SOBEL_H

#include "hocl.h"

/**
* Function to apply the sobel filter to an image of size width*height.
* The 1 pixel-wide border of the image will not be computed.
*
* @param width
*        The width of the processed image
* @param width
*        The heigth of the processed image
* @param input
*        The input image
* @param output
*        The output image
*/
void sobel(int width, int height, IN unsigned char *input, OUT unsigned char *output);

#endif
