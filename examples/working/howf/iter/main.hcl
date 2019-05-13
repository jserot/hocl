type t;

actor f : t -> t;
actor i : unit -> t;
actor o : t -> unit;

net () = o (iter 3 f (i ()));
