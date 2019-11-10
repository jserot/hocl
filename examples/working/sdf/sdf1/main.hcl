-- How to define multi-rate actors
-- The [foo] actor here consumes 8 tokens and produces 1 token per iteration

type int;

#pragma code("inp", "include/input.h", "input", "inputInit")
#pragma code("outp", "include/output.h", "output", "outputInit")
#pragma code("foo", "include/foo.h", "foo")

actor inp in () out(o: int[8]);
actor foo in (i: int[8]) out (o: int);
actor outp in (i: int) out ();

let _ = inp |> foo >> outp;
