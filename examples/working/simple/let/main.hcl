-- A reformulation of the example in [../basic] using a local "let" definition 
-- and a higher-order "main" wrapping function

type int;

actor i : unit -> int;
actor f : int -> int * int;
actor g : int -> int;
actor h : int * int -> int;
actor o : int -> unit;

net main x =
  let (y,z) = f x in
  h (g y, g z);

net _ = i |> main >> o;
