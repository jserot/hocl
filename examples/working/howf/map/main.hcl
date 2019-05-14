type t;
type t';

actor f : t -> t';
actor g : t' -> unit;
actor i : unit -> t;

net _ = i |> repl 3 >> map f >> map g;
