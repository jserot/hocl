-- A slighty less simple program, with multi-io actors

type t;

actor i in () out (o: t);
actor f in (i: t) out (o1: t, o2: t);
actor g in (i: t) out (o: t);
actor h in (i1: t, i2: t) out (o: t);
actor o in (i: t) out ();

let m (x,y) = h (g x, g y);
let () = i |> f >> m >> o;
