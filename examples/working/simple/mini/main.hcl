-- A minimalistic program

type int;

actor inp in () out(o: int);
actor f in (i: int) out (o: int);
actor outp in (i: int) out ();

let _ = inp |> f >> outp;
