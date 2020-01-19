-- A simple graph with an implicit broadcast node on the data flow
-- Insertion of bcast nodes is automatic when using the SystemC and the Preesm backends
-- When using the Dot backend, it is triggered by the [-insert_bcast] option

type int;

#pragma code("inp", "include/input.h", "input", "inputInit")
#pragma code("outp", "include/output.h", "output", "outputInit")
#pragma code("foo1", "include/foo.h", "foo")
#pragma code("foo2", "include/foo.h", "foo")

actor inp in () out(o: int);
actor foo1 in (i: int) out (o: int);
actor foo2 in (i: int) out (o: int);
actor outp in (i1: int, i2: int) out ();

let x = inp ();
let _ = outp (foo1 x, foo2 x);
