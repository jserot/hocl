-- This example shows how to mix tuples and bundles

type t;
type t';

actor i in () out (o1: t, o2:t, o3:t, o4:t);
actor f in (i:t) out (o: t);
actor o in (i1: t, i2:t, i3:t, i4:t) out ();

let _ = i |> map f >> o;
