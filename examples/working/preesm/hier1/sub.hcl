-- The subgraph to be used as refinement for node [sub] in [top.hcl]

type int;

#pragma code("foo", "../code/include/foo.h", "foo")
#pragma code("bar", "../code/include/foo.h", "bar")

source i: int;
sink o: int;

actor foo in (i: int) out (o: int);
actor bar in (i: int) out (o: int);

let _ = i |> foo >> bar >> o;
