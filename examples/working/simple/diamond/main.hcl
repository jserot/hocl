type t;

node f in (i: t data) out (o1: t data, o2: t data);
node g in (i: t data) out (o: t data);
node h in (i1: t data, i2: t data) out (o: t data);

graph top_f in (i: t data) out (o: t data)
fun
  val diamond left middle right v = 
    let (x,y) = left v in
    right (middle x, middle y)
  val o = diamond f g h i
end;


