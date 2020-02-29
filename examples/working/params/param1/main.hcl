-- A simple example with an actor taking a single, local parameter

type t;

node mult param (k: int) in (i: t) out (o: t);

graph top_s in (i: t) out (o: t)
struct
  node n: mult<2>(i)(o)
end;

graph top_f in (i: t) out (o: t)
fun
  val o = mult<2> i
end;
