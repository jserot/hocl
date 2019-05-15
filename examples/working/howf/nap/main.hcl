type t;

actor i1 in () out (o: t);
actor i2 in () out (o: t);
actor f in (i: t) out (o: t);
actor o in (i: t) out ();

net nap n f x = mapf (repl n f) x;

net [x1,x2,x3] = i1 |> nap 3 f; -- Parallel binding - first formulation

net _ = o x1;
net _ = o x2;
net _ = o x3;

net xs = i2 |> nap 3 f;         -- Alternate formulation
net _ = o (xs[0]);
net _ = o (xs[1]);
net _ = o (xs[2]);
