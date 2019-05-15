-- An example with an actor taking several parameters 

type int;

parameter width: nat = 128;
parameter flag: bool = true;

actor inp in () out(o: int);
actor f param (w: nat, u: bool) in (i: int) out (o: int);
actor outp in (i: int) out ();

net _ = inp |> f (width,flag) >> outp;
