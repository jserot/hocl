-- Choose your style...

type t;

actor g in (i: t) out (o: t);

graph top_s in (x: t) out (y: t) -- structural description (not generic)
struct
  wire w1: t
  wire w2: t
  wire w3: t
  wire w4: t
  wire w5: t
  node g1: g(x)(w1)
  node g2: g(w1)(w2)
  node g3: g(w2)(w3)
  node g4: g(w3)(w4)
  node g5: g(w4)(w5)
  node g6: g(w5)(y)
end;

graph top_f in (x: t) out (y: t) -- functional description (generic)
fun
  val rec iter n f x =
    if n=0 then x
    else iter (n-1) f (f x)
  val y = iter 6 g x
end;