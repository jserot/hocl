-- Dependant parameters

type int;

parameter baseFactor: nat = 2;
parameter multFactor: nat = baseFactor+1;

actor inp in () out(o: int);
actor foo param (k: nat) in (i: int) out (o: int);
actor outp in (i: int) out ();

let _ = inp |> foo multFactor >> outp;

#pragma code("inp", "include/input.h", "input", "inputInit")
#pragma code("outp", "include/output.h", "output", "outputInit")
#pragma code("foo", "include/foo.h", "foo")

