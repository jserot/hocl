/*
	============================================================================
	Name        : readYUV.c
	Author      : kdesnos
    Author      : mpelcat
    Modified by : jserot (for use in the HoCL framework)
	Version     : 1.1
	Copyright   : CECILL-C
	============================================================================
*/

#include "yuvRead.h"
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
// #include "clock.h"

/*========================================================================

   Global Variable

   ======================================================================*/
static FILE *ptfile ;

/*========================================================================

   initReadYUV DEFINITION

   ======================================================================*/
void yuvReadInit(int width, int height) {
    fprintf(stderr,"initReadYUV: opening file %s: width=%d height=%d\n", PATH, width, height);
    int fsize;
    if((ptfile = fopen(PATH, "rb")) == NULL )
    {
        fprintf(stderr,"ERROR: Task read cannot open yuv_file '%s'\n", PATH);
        return;
    }

#ifdef VERBOSE
    printf("Opened file '%s'\n", PATH);
#endif

    // Obtain file size:
    fseek (ptfile , 0 , SEEK_END);
    fsize = ftell (ptfile);
    rewind (ptfile);
    fprintf(stderr,"initReadYUV: opened file %s: fsize=%d\n", PATH, fsize);
    if(fsize < NB_FRAME*(width*height + width*height/2))
    {
        fprintf(stderr,"ERROR: Task read yuv_file incorrect size");
        return;
    }

#ifdef VERBOSE
    printf("Correct size for yuv_file '%s'\n", PATH);
#endif

    // Set initial clock
    // startTiming(0);
}

/*========================================================================

   readYUV DEFINITION

   ======================================================================*/
void yuvRead(int width, int height, unsigned char *y, unsigned char *u, unsigned char *v) {

    if( ftell(ptfile)/(width*height + width*height/2) >=NB_FRAME){
    	unsigned int time = 0;
        rewind(ptfile);
        /* time = stopTiming(0); */
        /* printf("\nMain: %d frames in %d us - %f fps\n", NB_FRAME-1 ,time, (NB_FRAME-1.0)/(float)time*1000000); */
        /* startTiming(0); */
    }
    int res = fread(y, sizeof(char), width * height, ptfile);
    res += fread(u, sizeof(char), width * height / 4, ptfile);
    res += fread(v, sizeof(char), width * height / 4, ptfile);
    if (res <= 0) {
        fprintf(stderr,"ERROR while reading");
        exit(1);
    }
}
