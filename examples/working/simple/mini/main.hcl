-- A minimalistic program

type int;

actor i : unit -> int;
actor f : int -> int;
actor o : int -> unit;

net _ = i |> f >> o;
