-- Inter-parameter dependencies (param-flow)

type pixel;

parameter width : nat = 128;
parameter fullwidth : nat = width*2; --1;

actor inp in () out (e: pixel);
actor foo param (w: nat) in (e: pixel) out (e: pixel);
actor outp in (e: pixel) out ();

net _ = outp (foo fullwidth (inp ()));
-- net _ = inp |> foo fullwidth >> outp;
