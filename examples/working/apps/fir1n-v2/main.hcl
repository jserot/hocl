-- This is a variant of [../fir1n-v1] with an explicit init input for the [MADD] chain

type float;

actor d in (i: float) out (o: float);
actor m in (i1: float, i2: float) out (o: float);
actor i in () out (o: float);
actor z in () out (o: float);
actor o in (i: float) out ();

net fir n z x = 
  let xs = miter n d x in
  foldl m z (x::xs);

net _ = i |> fir 3 (z()) >> o;
