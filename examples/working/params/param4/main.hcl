-- An example showing parameter passing via a wiring function 

type t;

node g in (k: int param, i: t) out (o: t);

node sub in (k: int param, i: t) out (o: t)
fun
  val o = i |> g 'k+1'
end;

graph top
in ( n1: int param = 1, n2: int param = 2, i1: t, i2: t)
out ( o1: t, o2: t)
fun
  val ff n x = x |> g n
  val o1 = ff n1 i1  -- Indirect parameter passing, via function [ff]
  val o2 = sub n2 i2 -- Direct parameter passing via a graph parameter
end;
