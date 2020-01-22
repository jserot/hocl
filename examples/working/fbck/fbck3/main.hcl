-- Describing cyclic wiring (feedback) - ex3

type t;
type t';

actor f in (i1: t, i2: t') out (o1: t, o2: t'); 
actor g in (i1: t', i2: t) out (o1: t', o2: t); 

graph top in (i1: t, i2: t) out (o1: t, o2: t)
fun
  val main (x1,x2) =
    let rec ((o1,z1),(z2,o2)) =
      f (x1, z2),
      g (z1, x2) in
    o1, o2
  val o = (i1,i2) |> main
end;
