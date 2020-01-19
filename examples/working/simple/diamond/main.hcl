-- A reformulation of the example in [../basic] with a higher-order wiring function

type t;

actor f in (i: t) out (o1: t, o2: t);
actor g in (i: t) out (o: t);
actor h in (i1: t, i2: t) out (o: t);

graph top_f in (i: t) out (o: t)
fun
  val diamond left middle right v = 
    let (x,y) = left v in
    right (middle x, middle y)
  val o = diamond f g h i
end;


