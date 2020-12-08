node f in (i1: int, i2: int) out (o1: int, o2: int);
node delay in (i: $a) out (o: $a);

graph delayed_cycle
   in (i: int)
  out (o: int)
fun
  val rec (o,z) = f i (delay z)
end;

graph undelayed_cycle
   in (i: int)
  out (o: int)
fun
  val rec (o,z) = f i z
end;
