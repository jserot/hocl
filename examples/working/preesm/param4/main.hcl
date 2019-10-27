type int;

parameter p: nat = 2;

actor inp in () out(o: int);
actor foo param (k: nat) in (i: int) out (o: int);
actor bar param (k: nat) in (i: int) out (o: int);
actor outp in (i: int) out ();

#pragma code("inp", "include/input.h", "input", "inputInit")
#pragma code("outp", "include/output.h", "output", "outputInit")
#pragma code("foo", "include/foo.h", "foo")
#pragma code("bar", "include/bar.h", "bar")

let _ = inp |> foo p >> bar p >> outp;
