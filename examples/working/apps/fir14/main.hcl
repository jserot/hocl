-- Another formulation of a 1x4 FIR model using a pre-defined 1x4 combining actor
type t;

actor d: t -> t;
actor i : unit -> t;
actor comb4 : t*t*t*t -> t;
actor o : t -> unit;

net delay n x = x :: miter (n-1) d x;

net x = delay 4 (i());
net () = o (comb4 (x[0],x[1],x[2],x[3]));
