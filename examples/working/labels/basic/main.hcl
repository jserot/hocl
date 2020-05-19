node foo in (i1: int, i2: bool) out (o: int);

val f x = x;

graph top in (e1: int, e2: bool) out (o1: int, o2: int)
fun
  val o1 = foo i1:e1 i2:e2
  val o2 = foo i2:e2 i1:e1
end;
