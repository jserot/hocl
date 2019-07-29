-- A minimalistic program

type int;

#pragma code("inp", "include/input.h", "input", "inputInit")
#pragma code("outp", "include/output.h", "output", "outputInit")
#pragma code("foo", "include/foo.h", "foo")

actor inp in () out(o: int);
actor foo in (i: int) out (o: int);
actor outp in (i: int) out ();

let _ = inp |> foo >> outp;
