-- The subgraph to be used as refinement for node [sub] in [top.hcl]

type int;


#pragma code("foo", "include/foo.h", "foo")
#pragma code("bar", "include/foo.h", "bar")

parameter k: nat; -- will be provided by upper hierarchy level
source i: int;
sink o: int;

actor foo param (k: nat) in (i: int) out (o: int);
actor bar in (e: int) out (s: int);

let _ = i |> foo k >> bar >> o;
