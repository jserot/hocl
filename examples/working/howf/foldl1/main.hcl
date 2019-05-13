type t;

actor f : t -> t;
actor g : t * t -> t;
actor i : unit -> t;
actor z : unit -> t;
actor o : t -> unit;

net () = o (foldl1 g (repl 5 (i ())));
