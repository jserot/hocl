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

net xs = mapf [f1,f2,f3] (i ());
net ys = shuffle [2,1,0] xs;
net () = o1 (ys[0]);
net () = o2 (ys[1]);
net () = o3 (ys[2]);
