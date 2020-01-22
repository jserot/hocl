-- A simple graph with an explicit broadcast node on the data flow
-- As opposed to implicit bcast nodes explicit bcasts can take parameters

type t;

actor foo1 in (i: t) out (o: t);
actor foo2 in (i: t) out (o: t);
bcast bc param (k: int) in (i: t) out (o1: t, o2: t);

graph top param (k1: int) in (i: t) out (o1: t, o2: t)
fun
  val (x1,x2) = i |> bc<k1>
  val (o1,o2) = foo1 x1, foo2 x2
end;

-- #pragma code("inp", "include/input.h", "input", "inputInit")
-- #pragma code("outp", "include/output.h", "output", "outputInit")
-- #pragma code("foo1", "include/foo.h", "foo")
-- #pragma code("foo2", "include/foo.h", "foo")
-- #pragma code("bc", "include/my_bcast.h", "bcast")

