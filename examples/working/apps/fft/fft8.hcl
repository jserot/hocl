-- A radix-8 FFT expressed using radix-4 and radix-2 FFTs
-- TODO : generalize to radix-2^n 
-- TODO : do not expand fft4 subnetwork

type cplx;

node fft2 in (x0: cplx, x1: cplx) out (y0: cplx, y1: cplx);

graph top
  in (x0: cplx, x1: cplx, x2: cplx, x3: cplx, x4: cplx, x5: cplx, x6: cplx, x7: cplx)
  out (y0: cplx, y1: cplx, y2: cplx, y3: cplx, y4: cplx, y5: cplx, y6: cplx, y7: cplx)
fun
  val fft4 (x0,x1,x2,x3) = 
    let (y0,y1) = fft2 (x0,x2) in
    let (y2,y3) = fft2 (x1,x3) in
    (y0,y1,y2,y3)
  val fft8 (x0,x1,x2,x3,x4,x5,x6,x7) = 
    let (y0,y1,y2,y3) = fft4 (x0,x2,x4,x6) in
    let (y4,y5,y6,y7) = fft4 (x1,x3,x5,x7) in
    let (z0,z4) = fft2 (y0,y4) in
    let (z1,z5) = fft2 (y1,y5) in
    let (z2,z6) = fft2 (y2,y6) in
    let (z3,z7) = fft2 (y3,y7) in
    (z0,z1,z2,z3,z4,z5,z6,z7)
  val (y0,y1,y2,y3,y4,y5,y6,y7) = fft8 (x0,x1,x2,x3,x4,x5,x6,x7)
end;
