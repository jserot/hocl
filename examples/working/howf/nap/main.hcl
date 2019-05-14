type t;

actor f : t -> t;
actor i1 : unit -> t;
actor i2 : unit -> t;
actor o : t -> unit;

net [x1,x2,x3] = i1 |> nap 3 f; -- Parallel binding - first formulation

net _ = o x1;
net _ = o x2;
net _ = o x3;

net xs = i2 |> nap 3 f;         -- Alternate formulation
net _ = o (xs[0]);
net _ = o (xs[1]);
net _ = o (xs[2]);
