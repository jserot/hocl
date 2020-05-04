-- Another model for a 1xN FIR filter with a single actor tap
-- This version uses the [iter] higher-order wiring functions

type float;

node tap in (x: float, z: float) out (y: float, zz:float);
node z in () out (o: float);

graph top in (i: float) out (zz: float, o: float)
fun
  val fir n z x = iter n tap (x,z)
  val zz,o = i |> fir 3 (z())
end;
