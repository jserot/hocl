-- A simple example with a single parameter

type int;

parameter multFactor: nat = 2;

actor inp in () out(o: int);
actor foo param (k: nat) in (i: int) out (o: int);
actor outp in (i: int) out ();

let _ = inp |> foo multFactor >> outp;

#pragma code("inp", "include/input.h", "input", "inputInit")
#pragma code("outp", "include/output.h", "output", "outputInit")
#pragma code("foo", "include/foo.h", "foo")

