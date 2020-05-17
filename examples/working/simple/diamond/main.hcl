type t;

node f in (i: t) out (o1: t, o2: t);
node g in (i: t) out (o: t);
node h in (i1: t, i2: t) out (o: t);

graph top_f in (i: t) out (o: t)
fun
  val diamond left middle right v = 
    let (x,y) = left v in
    right (middle x) (middle y)
  val o = diamond f g h i
end;


