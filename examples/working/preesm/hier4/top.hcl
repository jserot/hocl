-- Another program with one level of hierarchy and parameter-based configuration

type int;

parameter kt: nat = 2;

#pragma code("inp", "include/input.h", "input", "inputInit")
#pragma code("outp", "include/output.h", "output", "outputInit")
#pragma code("sub", "sub.hcl")

actor inp in () out(o: int);
graph sub param (ks: nat) in (i: int) out (o: int);
actor outp param (k: nat) in (i: int) out ();

let _ = inp |> sub kt >> outp kt;
