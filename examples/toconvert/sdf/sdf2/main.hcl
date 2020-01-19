-- How to define actors with parametric CP-rates
-- The [foo] actor here consumes [n] tokens and produces 1 token per iteration

type int;

#pragma code("inp", "include/input.h", "input", "inputInit")
#pragma code("outp", "include/output.h", "output", "outputInit")
#pragma code("foo", "include/foo.h", "foo")

parameter n: nat = 8;

actor inp param (k: nat) in () out(o: int[k]);
actor foo param (k: nat) in (i: int[k]) out (o: int);
actor outp in (i: int) out ();

let _ = inp n |> foo n >> outp;
