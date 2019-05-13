type t;
type t';

actor f1 : t -> t';
actor f2 : t -> t';
actor f3 : t -> t';
actor g : t' -> unit;
actor i : unit -> t;
actor o1 : t' -> unit;
actor o2 : t' -> unit;
actor o3 : t' -> unit;

net xs = map2f [f1,f2,f3] (repl 3 (i ()));
net () = o1 (xs[0]);
net () = o2 (xs[1]);
net () = o3 (xs[2]);
