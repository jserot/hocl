type t;

actor f : t -> t;
actor g : t * t -> t;
actor i : unit -> t;
actor z : unit -> t;
actor o : t -> unit;

net _ = i |> repl 5 >> foldl1 g >> o;
