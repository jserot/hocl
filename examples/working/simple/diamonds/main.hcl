-- How to use higher-order wiring functions to describe complex graphs

type t;

actor i in () out (o: t);
actor o in (i: t) out ();
actor f in (i: t) out (o1: t, o2: t);
actor g in (i: t) out (o: t);
actor h in (i1: t, i2: t) out (o: t);

net diamond top middle bottom v = 
  let (x,y) = top v in
  bottom (middle x, middle y);

net _ = i |> diamond f (diamond f (diamond f g h) h) h >> o;
