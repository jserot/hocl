-- Broadcasted parameter
-- Insertion of bcast actors is turned off by default. Use option [-insert_bcasts] to insert them.
-- Insertion is automatic when using the SystemC backend 

type int;

parameter p: nat = 2;

actor inp in () out(o: int);
actor foo param (k: nat) in (i: int) out (o: int);
actor bar param (k: nat) in (i: int) out (o: int);
actor outp in (i: int) out ();

let _ = inp |> foo p >> bar p >> outp;

#pragma code("inp", "include/input.h", "input", "inputInit")
#pragma code("outp", "include/output.h", "output", "outputInit")
#pragma code("foo", "include/foo.h", "foo")
#pragma code("bar", "include/bar.h", "bar")

