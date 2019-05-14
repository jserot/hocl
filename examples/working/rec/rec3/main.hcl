-- Recursive wiring - ex 3

type t;
type t';

actor f : t*t' -> t*t'; 
actor g : t'*t -> t'*t; 
actor i : unit -> t*t;
actor o : t*t -> unit;

net io f i o = o (f (i ()));

net main (x1,x2) =
  let rec ((o1,z1),(z2,o2)) =
    f (x1, z2),
    g (z1, x2) in
  o1, o2;

net _ = i |> main >> o;
