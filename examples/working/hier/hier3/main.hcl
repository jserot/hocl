-- A simple program with one level of hierarchy and local parameters

type t;

actor foo param (k: int) in (i: t) out (o: t);
actor bar in (e: t) out (s: t);

graph sub_s in (i: t) out (o: t)
struct
  wire w: t
  node n1: foo<1>(i)(w)
  node n2: bar(w)(o)
end;

graph top_s in (i: t) out (o: t)
struct
  node n: sub_s(i)(o)
end;

graph sub_f in (i: t) out (o: t)
fun
  val o = bar (foo<1> i)
end;

graph top_f in (i: t) out (o: t)
fun
  val o = sub_f i
end;

-- #pragma code("inp", "include/input.h", "input", "inputInit")
-- #pragma code("outp", "include/output.h", "output", "outputInit")
-- #pragma code("sub", "sub.hcl")

