type t;

actor f : t -> t;
actor i : unit -> t;
actor o : t -> unit;

net ys = miter 3 f (i ());
net () = o (ys[0]);
net () = o (ys[1]);
net () = o (ys[2]);
