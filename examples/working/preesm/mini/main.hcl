-- A minimalistic program

type int;

#pragma preesm("inp", "include/input.h", "input", "inputInit")
#pragma preesm("outp", "include/output.h", "output", "outputInit")
#pragma preesm("foo", "include/foo.h", "foo")

actor inp in () out(o: int);
actor foo in (i: int) out (o: int);
actor outp in (i: int) out ();

net _ = inp |> foo >> outp;
