-- This is a variant of [../fir1n-v1] with an explicit init input for the [MADD] chain

type float;

node d in (i: float) out (o: float);
node m in (i1: float, i2: float) out (o: float);
node z in () out (o: float);

graph top in (i: float) out (o: float)
fun
  val fir n z x = 
    let xs = miter n d x in
    foldl m z (x::xs)
  val o = i |> fir 3 (z())
end;
