-- An example showing parameter passing via a wiring function 

type t;

node g param (k: int) in (i: t) out (o: t);

node sub param (k: int) in (i: t) out (o: t)
fun
  val o = g (k+1) i
end;

graph top param (n1: int = 1, n2: int = 2) in (i1: t, i2:t) out (o1: t, o2: t)
fun
  val ff n x = g (n+1) x
  val o1 = ff n1 i1  -- Indirect parameter passing, via function [ff]
  val o2 = i2 |> sub n2 -- Direct parameter passing via a graph parameter
end;
