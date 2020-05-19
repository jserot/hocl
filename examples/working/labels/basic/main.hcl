-- This simple example shows the usage of labeled arguments 

node foo in (x: int, y: bool) out (o:  bool);

graph top in (i1: int, i2: bool) out (o1: bool, o2:bool, o3:bool)
fun
  val o1 = foo i1 i2        -- First formulation: port binding is here done by position (i1->x, i2->y)
  val o2 = foo x:i1 y:i2    -- Second formulation: port binding by name (label)
  val o3 = foo y:i2 x:i1    -- With name-based binding, order does not matter any longer
end;

