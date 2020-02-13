-- Another formulation of a 1x4 FIR model using a pre-defined 1x4 combining actor

type sint16;

node d in (i: sint16) out (o: sint16);
node comb4 in (i1: sint16, i2: sint16, i3: sint16, i4:sint16) out (o: sint16);

graph top in (i: sint16) out (o: sint16)
fun
  val shift n x = x :: miter (n-1) d x
  val x = i |> shift 4
  val o = comb4 (x[0],x[1],x[2],x[3])
end;
