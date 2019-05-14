-- Model for a 1xN FIR filter using separated [D]elay and [M]add actors
-- This version uses the [miter] and [foldl] higher-order wiring functions

type float;

actor d: float -> float;
actor m : float * float -> float;
actor i : unit -> float;
actor z : unit -> float;
actor o : float -> unit;

net fir n x = 
  let xs = miter n d x in
  foldl m x xs;

net _ = i |> fir 3 >> o;
