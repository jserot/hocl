type int;

parameter multFactor: nat = 2;
parameter incrFactor: nat = 1;

actor inp in () out(o: int);
actor foo param (k: nat, l:nat) in (i: int) out (o: int);
actor outp in (i: int) out ();

#pragma code("inp", "include/input.h", "input", "inputInit")
#pragma code("outp", "include/output.h", "output", "outputInit")
#pragma code("foo", "include/foo.h", "foo")

let _ = inp |> foo (multFactor,incrFactor) >> outp;
