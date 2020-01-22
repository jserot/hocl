-- A simple graph with an implicit broadcast node on the data flow
-- Insertion of bcast nodes is automatic when using the SystemC and the Preesm backends
-- Otherwise, it is triggered by the [-insert_bcast] option

type t;

actor inp in () out (o: t);
actor foo in (i: t) out (o: t);

graph top in () out (o1: t, o2: t)
fun
  val x = inp ()
  val (o1, o2) = foo x, foo x
end;

-- #pragma code("inp", "include/input.h", "input", "inputInit")
-- #pragma code("outp", "include/output.h", "output", "outputInit")
-- #pragma code("foo1", "include/foo.h", "foo")
-- #pragma code("foo2", "include/foo.h", "foo")

