-- Model for a 1xN FIR filter using separated [D]elay and [M]add actors
-- This version uses the [miter] and [foldl] higher-order wiring functions

type float;

actor d in (i: float) out (o: float);
actor m in (i1: float, i2: float) out (o: float);
actor z in () out (o: float);

graph top in (i: float) out (o: float)
fun
  val fir n x = 
    let xs = miter n d x in
    foldl m x xs
 val o = i |> fir 3
end;
