-- How to use higher-order wiring functions to describe complex graphs

type t;

actor i : unit -> t;
actor f : t -> t * t;
actor g : t -> t;
actor h : t * t -> t;
actor o : t -> unit;

net diamond top middle bottom v = 
  let (x,y) = top v in
  bottom (middle x, middle y);

net _ = i |> diamond f (diamond f (diamond f g h) h) h >> o;
