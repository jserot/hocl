-- This application computes and displays the image difference (Ik-Ik-1) on a YUV video stream
-- This is a HoCL reformulation of the Preesm example available at :
--   https://github.com/preesm/preesm-apps/tree/master/org.ietr.preesm.difference (difference2)
-- Note : the use of an explicit broadcast actor is required by the Preesm backend here

type uchar;

parameter width: nat = 352;
parameter height: nat = 288;
parameter ival: nat = 0;
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

actor Diff
  param (width: nat, height: nat)
  in (input: uchar[height*width], previous: uchar[height*width])
  out (result: uchar[height*width])
;

delay Delay
  param (width: nat, height: nat, ival: nat)
  in (input: uchar[height*width])
  out (output: uchar[height*width])
;

bcast Bc1
  param (width: nat, height: nat)
  in (i: uchar[height*width])
  out (o1: uchar[height*width], o2:uchar[height*width])
;

let (y,u,v) = Read_YUV(width, height) ();
let (y1,y2) = Bc1 (width,height) y;
let yp = Delay (width,height,ival) y1;
let yo = Diff (width, height) (y2,yp); 
let () = Display_YUV(index, width, height) (yo,u,v);

#pragma code("Read_YUV", "include/yuvRead.h", "readYUV", "initReadYUV")
#pragma code("Diff", "include/difference.h", "difference")
#pragma code("Display_YUV", "include/yuvDisplay.h", "yuvDisplay", "yuvDisplayInit")
#pragma code("Bc1", "include/broadcast.h", "bcast")

