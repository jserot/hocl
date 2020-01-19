-- A radix-8 FFT expressed using radix-4 and radix-2 FFTs
-- TODO : generalize to radix-2^n 
-- TODO : do not expand fft4 subnetwork

type t;

actor fft2 in (x0: t, x1: t) out (y0: t, y1: t);
actor inp in () out (x0:t, x1:t, x2:t, x3:t, x4:t, x5:t, x6:t, x7:t);
actor outp in (x0:t, x1:t, x2:t, x3:t, x4:t, x5:t, x6:t, x7:t) out ();

let fft4 (x0,x1,x2,x3) = 
  let (y0,y1) = fft2 (x0,x2) in
  let (y2,y3) = fft2 (x1,x3) in
  (y0,y1,y2,y3);

let fft8 (x0,x1,x2,x3,x4,x5,x6,x7) = 
  let (y0,y1,y2,y3) = fft4 (x0,x2,x4,x6) in
  let (y4,y5,y6,y7) = fft4 (x1,x3,x5,x7) in
  let (z0,z4) = fft2 (y0,y4) in
  let (z1,z5) = fft2 (y1,y5) in
  let (z2,z6) = fft2 (y2,y6) in
  let (z3,z7) = fft2 (y3,y7) in
  (z0,z1,z2,z3,z4,z5,z6,z7);

let _ = inp |> fft8 >> outp;
