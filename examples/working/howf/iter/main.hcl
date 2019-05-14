type t;

actor f : t -> t;
actor i : unit -> t;
actor o : t -> unit;

net _ = i |> iter 3 f >> o;
