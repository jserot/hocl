-- A slighty less simple program, with multi-io actors

type t;

actor i : unit -> t;
actor f : t -> t * t;
actor g : t -> t;
actor h : t * t -> t;
actor o : t -> unit;

-- net (x,y) = f (i ());
-- net () = o (h (g x, g y));
net (x,y) = i |> f;
net () = (x,y) >> h (g x, g y) >> o;
