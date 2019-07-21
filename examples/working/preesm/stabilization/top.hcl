type uchar;

parameter width: nat = 360;
parameter height: nat = 202;
parameter id: nat = 0;
parameter border: nat = 100;
parameter displayWidth: nat = width+2*border;
parameter displayHeight: nat = height+2*border;
parameter displaySize: nat = displayHeight*displayWidth;

#pragma code("ReadYUV", "include/yuvRead.h", "readYUV", "initReadYUV")
#pragma code("DisplayYUV", "include/yuvDisplay.h", "yuvDisplay", "yuvDisplayInit")
#pragma code("MD5", "include/md5.h", "MD5_Update")
#pragma code("WriteYUV", "include/yuvWrite.h", "yuvWrite", "initYUVWrite")
#pragma code("DuplicateY", "include/bcasts.h", "duplicateY")
#pragma code("DuplicateU", "include/bcasts.h", "duplicateU")
#pragma code("DuplicateV", "include/bcasts.h", "duplicateV")

actor ReadYUV
  param (width: nat, height: nat)
  in ()
  out (y: uchar "height*width", u: uchar "height/2*width/2", v: uchar "height/2*width/2")
;

actor DisplayYUV
  param (id: nat, height: nat, width: nat, border:nat)
  in (y: uchar "height*width", u: uchar "height/2*width/2", v: uchar "height/2*width/2")
  out ()
;

actor Stabilization
  param (width: nat, height: nat, border: nat)
  in (y: uchar "height*width",
      u: uchar "height/2*width/2",
      v: uchar "height/2*width/2")
 out (rY: uchar "(height+2*border)*(width+2*border)",
      rU: uchar "(height+2*border)/2*(width+2*border)/2",
      rV: uchar "(height+2*border)/2*(width+2*border)/2")
;

actor MD5
  param (size: nat)
  in (data: uchar "size")
  out ()
;

actor WriteYUV
  param (width: nat, height: nat)
  in (y: uchar "height*width", u: uchar "height/2*width/2", v: uchar "height/2*width/2")
  out ()
;

actor DuplicateY
  param (size: nat)
  in (inp: uchar "size")
  out (out_0: uchar "size", out_1: uchar "size", out_2: uchar "size") 
;

actor DuplicateU
  param (size: nat)
  in (inp: uchar "size/4")
  out (out_0: uchar "size/4", out_1: uchar "size/4")
;

actor DuplicateV
  param (size: nat)
  in (inp: uchar "size/4")
  out (out_0: uchar "size/4", out_1: uchar "size/4")
;

let (y,u,v) = ReadYUV(width, height) ();
let (rY,rU,rV) = Stabilization(width,height,border) (y,u,v);
let (y0,y1,y2) = DuplicateY(displaySize) rY;
let (u0,u1) = DuplicateU(displaySize) rU;
let (v0,v1) = DuplicateV(displaySize) rV;
let () = DisplayYUV(id,displayHeight,displayWidth,border) (y0,u0,v0);
let () = MD5(displaySize) y1;
let () = WriteYUV(displayWidth,height) (y2,u1,v1);
