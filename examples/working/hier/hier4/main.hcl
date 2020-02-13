-- A simple program with one level of hierarchy and inter-level parameter passing

type t;

node foo param (k: int) in (i: t) out (o: t);
node bar in (e: t) out (s: t);

node sub_s param (p: int) in (i: t) out (o: t)
struct
  wire w: t
  node n1: foo<p>(i)(w)
  node n2: bar(w)(o)
end;

node sub_f param (p: int) in (i: t) out (o: t)
fun
  val o = bar (foo<p> i)
end;

graph top_s param (k:int=2) in (i: t) out (o: t)
struct
  node n: sub_s<k>(i)(o)
end;

graph top_f param (k:int=2) in (i: t) out (o: t)
fun
  val o = sub_f<k> i
end;

-- #pragma code("inp", "include/input.h", "input", "inputInit")
-- #pragma code("outp", "include/output.h", "output", "outputInit")
-- #pragma code("sub", "sub.hcl")

