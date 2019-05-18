type t;

actor i1 in () out (o: t);
actor i2 in () out (o: t);
actor f in (i: t) out (o: t);
actor o in (i: t) out ();

let nap n f x = mapf (repl n f) x;

let [x1,x2,x3] = i1 |> nap 3 f; -- Parallel binding - first formulation

let _ = o x1;
let _ = o x2;
let _ = o x3;

let xs = i2 |> nap 3 f;         -- Alternate formulation
let _ = o (xs[0]);
let _ = o (xs[1]);
let _ = o (xs[2]);
