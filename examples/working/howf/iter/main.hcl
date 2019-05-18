type t;

actor f in (i: t) out (o: t);
actor i in () out (o: t);
actor o in (i: t) out ();

let _ = i |> iter 3 f >> o;
