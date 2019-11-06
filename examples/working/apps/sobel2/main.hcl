-- Basic edge extraction on the Y channel using the Sobel operator
-- Parallel version (as available in the Preesm tutorial directory)
-- This is a HoCL reformulation of the Preesm example available at :
--   https://github.com/preesm/preesm-apps/tree/master/tutorials/org.ietr.preesm.sobel

type uchar;

parameter width: nat = 352;
parameter height: nat = 288;
parameter index: nat = 0;
parameter nbSlice: nat = 2;
parameter sliceHeight: nat = (height/nbSlice)+2;

actor Read_YUV
  param (width: nat, height: nat)
  in ()
  out (y: uchar[height*width], u: uchar[height/2*width/2], v: uchar[height/2*width/2])
;

actor Split param (nbSlice: nat, width: nat, height: nat)
  in (input: uchar[height*width])
  out (output: uchar[nbSlice*width*(height/nbSlice+2)])
;

actor Sobel
  param (width: nat, height: nat)
  in (input: uchar[height*width])
  out (output: uchar[height*width])
;

actor Merge
  param (nbSlice: nat, width: nat, height: nat)
  in (input: uchar[nbSlice*width*(height/nbSlice+2)])
  out (output: uchar[height*width])
;

actor Display_YUV
  param (id: nat, width: nat, height: nat)
  in (y: uchar[height*width], u: uchar[height/2*width/2], v: uchar[height/2*width/2])
  out ()
;

let (yi,u,v) = Read_YUV(width, height) ();
let yo = yi
       >> Split (nbSlice, width, height)
       >> Sobel (width, sliceHeight)
       >> Merge (nbSlice, width, height);
let () = Display_YUV (index, width, height) (yo,u,v);

#pragma code("Read_YUV", "include/yuvRead.h", "readYUV", "initReadYUV")
#pragma code("Merge", "include/splitMerge.h", "merge")
#pragma code("Sobel", "include/sobel.h", "sobel")
#pragma code("Split", "include/splitMerge.h", "split")
#pragma code("Display_YUV", "include/yuvDisplay.h", "yuvDisplay", "yuvDisplayInit")

