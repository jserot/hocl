type int;

parameter baseFactor: nat = 2;
parameter multFactor: nat = baseFactor+1;

actor inp in () out(o: int);
actor foo param (k: nat) in (i: int) out (o: int);
actor outp in (i: int) out ();

#pragma code("inp", "../code/include/input.h", "input", "inputInit")
#pragma code("outp", "../code/include/output.h", "output", "outputInit")
#pragma code("foo", "../code/include/foo.h", "foo")

net _ = inp |> foo multFactor >> outp;
