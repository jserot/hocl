type t;

actor f : t -> unit;
actor i : unit -> t;
actor o : t -> unit;

net xs = repl 3 (i ());

net () = o (xs[0]);
net () = o (xs[1]);
net () = o (xs[2]);
-- net () = o (xs[3]); -- Raises "invalid list index"
