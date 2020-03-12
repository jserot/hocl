-- A simple graph with a simple, pure feedback

node foo in (i1: int, i2: int) out (o1: int, o2: int);

graph top in (i: int) out (o: int)
fun
  -- val o = let rec (y,z) = foo (i,z) in y
  val rec (o,z) = foo (i,z)
end;

