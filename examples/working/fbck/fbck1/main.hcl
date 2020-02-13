-- Describing cyclic wiring (feedback) - ex1

node foo in (i1: int, i2: int) out (o1: int, o2: int); 

graph top in (i: int) out (o: int)
fun
  val cyc f x = 
    let rec (y,z) = f (x,z) in
    y
  val o = i |> cyc foo
end;
