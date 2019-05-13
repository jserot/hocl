type t;

actor f1 : t -> t;
actor f2 : t -> t;
actor f3 : t -> t;
actor i : unit -> t;
actor o : t -> unit;

net () = o (pipe [f1,f2,f3] (i ()));
