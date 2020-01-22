-- Describing cyclic wiring (feedback) - ex1

actor f in (i1: int, i2: int) out (o1: int, o2: int); 

graph top in (i: int) out (o: int)
fun
  val main x = 
    let rec (y,z) = f (x,z) in
    y
  val o = i |> main
end;
