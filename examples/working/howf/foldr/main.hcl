type t;

actor f : t -> t;
actor g : t * t -> t;
actor i : unit -> t;
actor z : unit -> t;
actor o : t -> unit;

net _ = i |> repl 3 >> foldr g (z()) >> o;
