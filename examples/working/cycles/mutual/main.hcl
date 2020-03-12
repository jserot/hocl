-- Describing cyclic wiring (feedback) - ex3

type t;
type t';

node f in (i1: t, i2: t') out (o1: t, o2: t'); 
node g in (i1: t', i2: t) out (o1: t', o2: t); 

graph top in (i1: t, i2: t) out (o1: t, o2: t)
fun
  val rec ((o1,z1),(z2,o2)) = f (i1,z2), g (z1,i2)
end;
