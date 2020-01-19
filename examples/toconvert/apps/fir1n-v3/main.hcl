-- Another model for a 1xN FIR filter with a single actor tap
-- This version uses the [iter] higher-order wiring functions

type float;

actor tap in (x: float, z: float) out (y: float, zz:float);
actor i in () out (o: float);
actor z in () out (o: float);
actor o in (i: float) out ();

let fir n z x = iter n tap (x,z);

let z',y = i |> fir 3 (z());
let _ = o z';
let _ = o y;
