type int;

parameter multFactor: nat = 2;
parameter incrFactor: nat = 1;

actor inp in () out(o: int);
actor foo param (k: nat, l:nat) in (i: int) out (o: int);
actor outp in (i: int) out ();

#pragma code("inp", "../code/include/input.h", "input", "inputInit")
#pragma code("outp", "../code/include/output.h", "output", "outputInit")
#pragma code("foo", "../code/include/foo.h", "foo")

net _ = inp |> foo (multFactor,incrFactor) >> outp;
