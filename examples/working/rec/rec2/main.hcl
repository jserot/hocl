-- Recursive wiring - ex2

type t;
type t';

actor f in (i1: t, i2: t') out (o1: t, o2: t'); 
actor g in (i: t') out (o: t'); 
actor h in (i: t') out (o: t'); 
actor i in () out (o: t);
actor o in (i: t) out ();

let main x = 
  let rec (y,z) = f (x,h (g z)) in
  y;
 
let _ = i |> main >> o;
