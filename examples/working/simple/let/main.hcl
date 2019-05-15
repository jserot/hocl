-- A reformulation of the example in [../basic] using a local "let" definition 
-- and a higher-order "main" wrapping function

type int;

actor i in () out (o: int);
actor f in (i: int) out (o1: int, o2: int);
actor g in (i: int) out (o: int);
actor h in (i1: int, i2: int) out (o: int);
actor o in (i: int) out ();

net main x =
  let (y,z) = f x in
  h (g y, g z);

net _ = i |> main >> o;
