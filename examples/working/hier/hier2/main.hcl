type t;

node f in () out (o1: t, o2: t);
node h in (i1: t, i2: t) out ();
node s in (i: t) out (o: t);

node g_s in (i: t) out (o: t)
struct
  wire w: t
  node s1: s(i)(w)
  node s2: s(w)(o)
end;

node g_f in (i: t) out (o: t)
fun
  val o = s (s i)
end;

graph top_s in () out ()
struct
  wire w1: t 
  wire w2: t
  wire w3: t
  wire w4: t
  node f: f()(w1,w2)
  node g1: g_s(w1)(w3)
  node g2: g_s(w2)(w4)
  node h: h(w3,w4)()
end;

graph top_f in () out ()
fun
  val (x,y) = f ()
  val () = h (g_f x, g_f y)
end;
