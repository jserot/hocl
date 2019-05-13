-- A reformulation of the example in [../let] with a higher-order wiring function

type t;

actor i : unit -> t;
actor o : t -> unit;
actor f : t -> t * t;
actor g : t -> t;
actor h : t * t -> t;

net io f i o = o (f (i ()));

net diamond top middle bottom v = 
  let (x,y) = top v in
  bottom (middle x, middle y);

net () = io (diamond f g h) i o; 
