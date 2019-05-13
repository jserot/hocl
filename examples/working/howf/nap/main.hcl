type t;

actor f : t -> t;
actor i1 : unit -> t;
actor i2 : unit -> t;
actor o : t -> unit;

net [x1,x2,x3] = nap 3 f (i1 ()); -- Parallel binding - first formulation

net () = o x1;
net () = o x2;
net () = o x3;

net xs = nap 3 f (i2 ());          -- Alternate formulation
net () = o (xs[0]);
net () = o (xs[1]);
net () = o (xs[2]);
