-- A simple program with one level of hierarchy, w/o parameters

type t;

node foo in (i: t) out (o: t);
node bar in (e: t) out (s: t);

node sub_s in (i: t) out (o: t)
struct
  wire w: t
  node n1: foo(i)(w)
  node n2: bar(w)(o)
end;

node sub_f in (i: t) out (o: t)
fun
  val o = bar (foo i)
end;

graph top1 in (i: t) out (o: t)
fun
  val o = sub_s i 
end;

graph top2 in (i: t) out (o: t)
fun
  val o = sub_f i
end;


-- #pragma code("inp", "include/input.h", "input", "inputInit")
-- #pragma code("outp", "include/output.h", "output", "outputInit")
-- #pragma code("sub", "sub.hcl")

