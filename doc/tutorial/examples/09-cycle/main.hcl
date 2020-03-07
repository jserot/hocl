node f in (i1: int, i2: int) out (o1: int, o2: int); 
node g in (i: int) out (o: int); 

graph top_s in (i: int) out (o: int)
struct
  wire w1: int
  wire w2: int
  node n1: f(i,w1)(o,w2)
  node n2: g(w2)(w1)
end;

graph top_f in (i: int) out (o: int)
fun
  val o =
   let rec (y,z) = f (i, g z) in
   y
end;
