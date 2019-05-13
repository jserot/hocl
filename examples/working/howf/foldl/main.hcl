type t;

actor f : t -> t;
actor g : t * t -> t;
actor i : unit -> t;
actor z : unit -> t;
actor o : t -> unit;

net xs = repl 3 (i ());
net () = o (foldl g (z ()) xs);
