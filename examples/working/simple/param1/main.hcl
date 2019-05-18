-- A simple example with parameters

type int;

parameter width: nat = 128;

actor inp in () out(o: int);
actor f param (w: nat) in (i: int) out (o: int);
actor outp in (i: int) out ();

let _ = inp |> f width >> outp;
