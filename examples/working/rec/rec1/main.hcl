-- Recursive wiring - ex1

type int;

actor f : int*int -> int*int; 
actor i : unit -> int;
actor o : int -> unit;

net main x = 
  let rec (y,z) = f (x,z) in
  y;
 
net () = i |> main >> o;
