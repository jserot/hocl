-- How to use higher-order wiring functions to describe complex graphs
-- To be convinced, just write the equivalent structural description of graph [top]..

type t;

node f in (i: t) out (o1: t, o2: t);
node g in (i: t) out (o: t);
node h in (i1: t, i2: t) out (o: t);

graph top_f in (i: t) out (o: t)
fun
  val diamond left middle right v = 
    let (x,y) = left v in
    right (middle x, middle y)
  val o = diamond f (diamond f (diamond f g h) h) h i
end;

