-- Another example of parameter passing with two levels of hierarchy

type t;

actor f in () out (o1: t, o2: t);
actor h in (i1: t, i2: t) out ();
actor s param (k: int) in (i: t) out (o: t);

graph sub_s param (k: int) in (i: t) out (o: t)
struct
  wire w: t
  node s1: s<k-1>(i)(w)
  node s2: s<k+1>(w)(o)
end;

graph sub_f param (k: int) in (i: t) out (o: t)
fun
  val o = s<k+1> (s<k-1> i)
end;

graph top_s in () out ()
struct
  wire w1: t 
  wire w2: t
  wire w3: t
  wire w4: t
  node f: f()(w1,w2)
  node g1: sub_s<2>(w1)(w3)
  node g2: sub_s<4>(w2)(w4)
  node h: h(w3,w4)()
end;

graph top_f in () out ()
fun
  val (x,y) = f ()
  val () = h (sub_f<2> x, sub_s<4> y)
end;
