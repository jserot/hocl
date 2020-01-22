-- Describing cyclic wiring (feedback) - ex2

type t;
type t';

actor f in (i1: t, i2: t') out (o1: t, o2: t'); 
actor g in (i: t') out (o: t'); 
actor h in (i: t') out (o: t'); 

graph top in (i: t) out (o: t)
fun
  val main x = 
    let rec (y,z) = f (x,h (g z)) in
    y
  val o = i |> main
end;
