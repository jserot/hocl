type t;

actor f : t -> t;
actor i : unit -> t;
actor o : t -> unit;

net ys = i |> miter 3 f;
net _ = o (ys[0]);
net _ = o (ys[1]);
net _ = o (ys[2]);
