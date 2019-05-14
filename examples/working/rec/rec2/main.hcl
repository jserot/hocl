-- Recursive wiring - ex2

type t;
type t';

actor f : t*t' -> t*t'; 
actor g : t' -> t'; 
actor h : t' -> t'; 
actor i : unit -> t;
actor o : t -> unit;

net main x = 
  let rec (y,z) = f (x,h (g z)) in
  y;
 
net _ = i |> main >> o;
