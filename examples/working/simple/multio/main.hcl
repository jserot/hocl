-- A slighty less simple program, with multi-io actors

type t;

node f in () out (o1: t data, o2: t data);
node g in (i: t data) out (o: t data);
node h in (i1: t data, i2: t data) out ();

graph top_s in () out ()
struct
  wire w1, w2, w3, w4: t data
  box n1: f()(w1,w2)
  box n2: g(w1)(w3)
  box n3: g(w2)(w4)
  box n4: h(w3,w4)()
end;

graph top_f in () out ()
fun
  val (x1,x2) = f ()
  val () = h (g x1, g x2)
end;
