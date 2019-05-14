type t;

actor f : t -> unit;
actor i : unit -> t;
actor o : t -> unit;

net xs = i |> repl 3;

net _ = o (xs[0]);
net _ = o (xs[1]);
net _ = o (xs[2]);
-- net _ = o (xs[3]); -- Raises "invalid list index"
