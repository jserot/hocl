-- Model for a 1xN FIR filter using separated [D]elay and [M]add actors
-- This version uses the [miter] and [foldl] higher-order wiring functions

type float;

actor d in (i: float) out (o: float);
actor m in (i1: float, i2: float) out (o: float);
actor i in () out (o: float);
actor z in () out (o: float);
actor o in (i: float) out ();

net fir n x = 
  let xs = miter n d x in
  foldl m x xs;

net _ = i |> fir 3 >> o;
