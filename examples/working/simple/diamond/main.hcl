-- A reformulation of the example in [../let] with a higher-order wiring function

type t;

actor i in () out (o: t);
actor o in (i: t) out ();
actor f in (i: t) out (o1: t, o2: t);
actor g in (i: t) out (o: t);
actor h in (i1: t, i2: t) out (o: t);

let diamond top middle bottom v = 
  let (x,y) = top v in
  bottom (middle x, middle y);

let _ = i |> diamond f g h >> o;
