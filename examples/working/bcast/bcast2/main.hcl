-- A simple graph with an explicit broadcast node
-- As opposed to implicit bcast nodes explicit bcasts can take parameters

type int;

parameter k1: nat = 2;

#pragma code("inp", "include/input.h", "input", "inputInit")
#pragma code("outp", "include/output.h", "output", "outputInit")
#pragma code("foo1", "include/foo.h", "foo")
#pragma code("foo2", "include/foo.h", "foo")
#pragma code("bc", "include/my_bcast.h", "bcast")

actor inp in () out(o: int);
actor foo1 in (i: int) out (o: int);
actor foo2 in (i: int) out (o: int);
actor outp in (i1: int, i2: int) out ();
bcast bc param (k: nat) in (i: int) out (o1: int, o2: int);

let (x1,x2) = inp |> bc k1;
let _ = outp (foo1 x1, foo2 x2);
