node f in (i: int) out (o1: int, o2: int);
node g in (i: int) out (o:int);
node h in (i1:int, i2:int) out (o: int);

graph top1 in (i: int) out (o: int)
struct
  wire x1,x2,y1,y2: int
  node n1: f(i)(x1,x2)
  node n2: g(x1)(y1)
  node n3: g(x2)(y2)
  node n4: h(y1,y2)(o)
end;

graph top2 in (i: int) out (o: int)
fun
  val (x1,x2) = f i
  val o = h (g x1, g x2)
end;
