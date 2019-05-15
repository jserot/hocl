type t;

actor i in () out (o: t);
actor f in (i: t) out ();
actor o in (i: t) out ();

net xs = i |> repl 3;

net _ = o (xs[0]);
net _ = o (xs[1]);
net _ = o (xs[2]);
-- net _ = o (xs[3]); -- Raises "invalid list index"
