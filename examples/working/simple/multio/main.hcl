-- A slighty less simple program, with multi-io actors

type t;

node f in () out (o1: t, o2: t);
node g in (i: t) out (o: t);
node h in (i1: t, i2: t) out ();

graph top_s in () out ()
struct
  wire w1, w2, w3, w4: t
  node n1: f()(w1,w2)
  node n2: g(w1)(w3)
  node n3: g(w2)(w4)
  node n4: h(w3,w4)()
end;

graph top_f in () out ()
fun
  val (x1,x2) = f ()
  val () = h (g x1, g x2)
end;
