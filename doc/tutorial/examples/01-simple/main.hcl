node f in (i: int) out(o: int);
node g in (i: int) out(o: int);

-- First formulation
graph top1 in (i: int) out (o: int)
struct
  wire w: int
  node n1: f(i)(w)
  node n2: g(w)(o)
end;

-- Second formulation
graph top2 in (i: int) out (o: int)
fun
  val o = g (f i)
end;

-- Third formulation
graph top3 in (i: int) out (o: int)
fun
  val o = i |> f |> g
end;
