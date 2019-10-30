-- The subgraph to be used as refinement for node [sub] in [top.hcl]

type int;

#pragma code("foo", "include/foo.h", "foo")
#pragma code("bar", "include/foo.h", "bar")

source i: int;
sink o: int;

actor foo in (i: int) out (o: int);
actor bar in (e: int) out (s: int);

let _ = i |> foo >> bar >> o;