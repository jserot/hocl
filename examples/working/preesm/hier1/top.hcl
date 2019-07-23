-- A simple program with one level of hierarchy, w/o parameters

type int;

#pragma code("inp", "include/input.h", "input", "inputInit")
#pragma code("outp", "include/output.h", "output", "outputInit")
#pragma code("sub", "sub.hcl")

actor inp in () out(o: int);
graph sub in (i: int) out (o: int);
actor outp in (i: int) out ();

let _ = inp |> sub >> outp;
