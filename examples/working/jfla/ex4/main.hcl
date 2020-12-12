node f in (i1: int, i2: int) out (o1: int, o2: int);
node delay in (init: 'a param, i: 'a) out (o: 'a);

graph top
   in (i: int)
  out (o: int)
fun
  val rec (o,z) = f i (delay '0' z)
end;
