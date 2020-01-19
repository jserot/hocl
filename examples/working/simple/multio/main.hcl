-- A slighty less simple program, with multi-io actors

type t;

actor f in () out (o1: t, o2: t);
actor g in (i: t) out (o: t);
actor h in (i1: t, i2: t) out ();

graph top_s in () out ()
struct
  wire w1: t
  wire w2: t
  wire w3: t
  wire w4: t
  node n1: f()(w1,w2)
  node n2: g(w1)(w3)
  node n3: g(w2)(w4)
  node n4: h(w3,w4)()
end;

graph top_f in () out ()
fun
  val (x1,x2) = f ()
  val () = h (g x1, g x2)
  -- val m (x,y) = h (g x, g y)
  -- val () = i |> f >> m >> o
end;
