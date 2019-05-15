type pixel;

parameter width: nat = 352;
parameter height: nat = 288;
parameter index: nat = 0;
parameter nbslice: nat = 8;
parameter sliceheight: nat = height/nbslice+2;

actor readyuv
  param (width: nat, height: nat)
  in ()
  out (y: pixel, u: pixel, v: pixel)
;

actor split param (nbslice: nat, width: nat, height: nat)
  in (input: pixel)
  out ( output: pixel)
;

actor sobel
  param (width: nat, height: nat)
  in (input: pixel)
  out (output: pixel)
;

actor merge
  param (nbslice: nat, width: nat, height: nat)
  in (input: pixel)
  out (output: pixel)
;

actor display
  param (id: nat, width: nat, height: nat)
  in (y: pixel, u: pixel, v: pixel)
  out ()
;

net (yi,u,v) = readyuv(width, height) ();
net yo = yi
       >> split (nbslice, width, height)
       >> sobel (width, height)
       >> merge (nbslice, width, height);
net () = display (index, width, height) (yo,u,v);
