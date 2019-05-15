type t;

actor i in () out (o: t);
actor f in (i: t) out (o: t);
actor o in (i: t) out ();

net ys = i |> miter 3 f;
net _ = o (ys[0]);
net _ = o (ys[1]);
net _ = o (ys[2]);
