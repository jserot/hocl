-- The subgraph to be used as refinement for node [sub] in [top.hcl]

type int;

#pragma code("foo", "include/foo.h", "foo")
#pragma code("bar", "include/foo.h", "bar")

parameter ks: nat; -- will be provided by upper hierarchy level
source i(k: nat): int;
sink o(k: nat): int;

actor foo param (k: nat) in (i: int) out (o: int);
actor bar in (e: int) out (s: int);

let _ = i ks |> foo ks >> bar >> o ks;
