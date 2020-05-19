type bool;

node foo in (i1: int, i2: bool) out (o:  bool);

graph top in (i1: int, i2: bool) out (o1: bool, o2:bool, o3:bool)
fun
  val o1 = foo i1 i2
  val o2 = foo i1:i1 i2:i2
  val o3 = foo i2:i2 i1:i1
end;

