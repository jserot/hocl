type t;

actor i in () out (o: t);
actor f1 in (i: t) out (o: t);
actor f2 in (i: t) out (o: t);
actor f3 in (i: t) out (o: t);
actor o in (i: t) out ();

let _ = i |> pipe [f1,f2,f3] >> o;
