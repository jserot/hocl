-- How to use tuples with multi inputs and multi outputs actors

type int;

actor f : unit -> int * int;
actor g : int -> int;
actor h : int * int -> unit;

net (x,y) = f ();
net () = h (g x, g y);
