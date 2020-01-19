-- A simple program with one level of hierarchy and inter-level parameter passing

type t;

actor foo param (k: int) in (i: t) out (o: t);
actor bar in (e: t) out (s: t);

graph sub_s param (p: int) in (i: t) out (o: t)
struct
  wire w: t
  node n1: foo<p>(i)(w)
  node n2: bar(w)(o)
end;

graph top_s in (i: t) out (o: t)
struct
  node n: sub_s<2>(i)(o)
end;

graph sub_f param (p: int) in (i: t) out (o: t)
fun
  val o = bar (foo<p> i)
end;

graph top_f in (i: t) out (o: t)
fun
  val o = sub_f<2> i
end;

-- #pragma code("inp", "include/input.h", "input", "inputInit")
-- #pragma code("outp", "include/output.h", "output", "outputInit")
-- #pragma code("sub", "sub.hcl")

