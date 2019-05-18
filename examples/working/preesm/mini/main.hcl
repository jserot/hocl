-- A minimalistic program

type int;

#pragma code("inp", "../code/include/input.h", "input", "inputInit")
#pragma code("outp", "../code/include/output.h", "output", "outputInit")
#pragma code("foo", "../code/include/foo.h", "foo")

actor inp in () out(o: int);
actor foo in (i: int) out (o: int);
actor outp in (i: int) out ();

net _ = inp |> foo >> outp;
