-- Describing cyclic wiring (feedback) - ex3

type t1;
type t2;

node f in (i1: t1, i2: t2) out (o1: t1, o2: t2); 
node g in (i1: t2, i2: t1) out (o1: t2, o2: t1); 

graph top in (i1: t1, i2: t1) out (o1: t1, o2: t1)
fun
  val rec ((o1,z1),(z2,o2)) = f (i1,z2), g (z1,i2)
end;
