type t;
type t';

actor f : t -> t';
actor g : t' -> unit;
actor i : unit -> t;

net ys = map f (repl 3 (i ()));
net zs = map g ys;
