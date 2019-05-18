type t;

actor i in () out (o: t);
actor f in (i: t) out ();
actor o in (i: t) out ();

let xs = i |> repl 3;

let _ = o (xs[0]);
let _ = o (xs[1]);
let _ = o (xs[2]);
-- let _ = o (xs[3]); -- Raises "invalid list index"
