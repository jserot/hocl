node f in (i: int) out(o: int);

graph top1 in (i: int) out (o: int)
struct
  wire w1: int
  wire w2: int
  wire w3: int
  node n1: f(i)(w1)
  node n2: f(w1)(w2)
  node n3: f(w2)(w3)
  node n4: f(w3)(o)
end;

graph top2 in (i: int) out (o: int)
fun
  val o = i |> iter 4 f
end;
