type t;
type t';

actor i in () out (o: t);
actor f1 in (i: t) out (o: t');
actor f2 in (i: t) out (o: t');
actor f3 in (i: t) out (o: t');
actor g in (i: t') out ();
actor o1 in (i: t') out ();
actor o2 in (i: t') out ();
actor o3 in (i: t') out ();

net xs = i |> repl 3 >> map2f [f1,f2,f3];
net _ = o1 (xs[0]);
net _ = o2 (xs[1]);
net _ = o3 (xs[2]);
