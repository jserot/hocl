-- This is a variant of [../fir1n-v1] with an explicit init input for the [MADD] chain

type float;

actor d: float -> float;
actor m : float * float -> float;
actor i : unit -> float;
actor z : unit -> float;
actor o : float -> unit;

net fir n z x = 
  let xs = miter n d x in
  foldl m z (x::xs);

net _ = i |> fir 3 (z()) >> o;
