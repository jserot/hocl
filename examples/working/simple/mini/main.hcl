node foo in (i: int) out (o: int);

graph top in (i: int) out (o: int)
fun
  val o = foo i
end;
