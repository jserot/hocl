-- Basic edge extraction on the Y channel using the Sobel operator
-- Monolithic version

type uchar;

parameter width: nat = 352;
parameter height: nat = 288;
parameter index: nat = 0;

actor Read_YUV
  param (width: nat, height: nat)
  in ()
  out (y: uchar[height*width], u: uchar[height/2*width/2], v: uchar[height/2*width/2])
;

actor Display_YUV
  param (id: nat, width: nat, height: nat)
  in (y: uchar[height*width], u: uchar[height/2*width/2], v: uchar[height/2*width/2])
  out ()
;

actor Sobel
  param (width: nat, height: nat)
  in (input: uchar[height*width])
  out (output: uchar[height*width])
;

let (yi,u,v) = Read_YUV(width, height) ();
let yo = Sobel (width, height) yi;
let () = Display_YUV(index, width, height) (yo,u,v);

#pragma code("Read_YUV", "include/yuvRead.h", "readYUV", "initReadYUV")
#pragma code("Sobel", "include/sobel.h", "sobel")
#pragma code("Display_YUV", "include/yuvDisplay.h", "yuvDisplay", "yuvDisplayInit")

