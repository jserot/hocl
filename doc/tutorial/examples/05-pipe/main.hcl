node f1 in (i: int) out(o: int);
node f2 in (i: int) out(o: int);
node f3 in (i: int) out(o: int);

graph top1 in (i: int) out (o: int)
struct
  wire w1: int
  wire w2: int
  node n1: f1(i)(w1)
  node n2: f2(w1)(w2)
  node n3: f3(w2)(o)
end;

graph top2 in (i: int) out (o: int)
fun
  val o = i |> pipe [f1,f2,f3]
end;
