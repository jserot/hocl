type t;

actor f in (i: t) out (o: t);
actor g in (i1: t, i2: t) out (o: t);
actor i in () out (o: t);
actor z in () out (o: t);
actor o in (i: t) out ();

let _ = i |> repl 3 >> foldr g (z()) >> o;
