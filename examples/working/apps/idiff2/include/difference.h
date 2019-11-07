/*
	============================================================================
	Name        : difference.h
	Author      : mpelcat
	Version     : 1.1
	Copyright   : CeCILL-C
	Description : Difference between two images
	============================================================================
*/

#ifndef DIFFERENCE_H
#define DIFFERENCE_H

#include "preesm.h"

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
* @param difference
*        The difference image (input-previous) of size height*width
*/
void difference(int width, int height, IN unsigned char *input, IN unsigned char *previous, OUT unsigned char *result);

#endif
