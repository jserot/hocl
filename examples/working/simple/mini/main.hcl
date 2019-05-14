-- A minimalistic program

type int;

actor i : unit -> int;
actor f : int -> int;
actor o : int -> unit;

net (|>) i f = f (i ());
net (>>) x f = f x;

net () = i |> f >> o;
