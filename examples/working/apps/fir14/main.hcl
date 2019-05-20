-- Another formulation of a 1x4 FIR model using a pre-defined 1x4 combining actor
type t;

actor d in (i: t) out (o: t);
actor i in () out (o: t);
actor comb4 in (i1: t, i2: t, i3: t, i4:t) out (o: t);
actor o in (i: t) out ();

let delay n x = x :: miter (n-1) d x;

let x = i |> delay 4;
let _ = comb4 (x[0],x[1],x[2],x[3]) >> o;
