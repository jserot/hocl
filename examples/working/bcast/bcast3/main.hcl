-- Parameters are implicitely broadcasted (IOW, the output port of a parameter box 
-- can be directly wired to several distinct input ports)
-- This interpretation is naturally supported by the Preesm backend
-- When using the SystemC backend, explicit bcast nodes are automatically inserted 
-- Inserted bcast nodes wan be viewed with the Dot backend by passing the [-insert_bcasts] option

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

