-- Another model for a 1xN FIR filter with a single actor tap
-- This version uses the [iter] higher-order wiring functions

type float;

actor tap: float * float -> float * float;
actor i : unit -> float;
actor z : unit -> float;
actor o : float -> unit;

net fir n z x = iter n tap (x,z);

net z',y = i |> fir 3 (z());
net _ = o z';
net _ = o y;
