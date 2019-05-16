type int;

parameter multFactor: nat = 2;

actor inp in () out(o: int);
actor foo param (k: nat) in (i: int) out (o: int);
actor outp in (i: int) out ();

#pragma preesm("inp", "include/input.h", "input", "inputInit")
#pragma preesm("outp", "include/output.h", "output", "outputInit")
#pragma preesm("foo", "include/foo.h", "foo")

net _ = inp |> foo multFactor >> outp;
