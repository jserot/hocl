-- Describing cyclic wiring (feedback) - ex1

type int;

actor f in (i1: int, i2: int) out (o1: int, o2: int); 
actor i in () out (o: int);
actor o in (i1: int) out ();

let main x = 
  let rec (y,z) = f (x,z) in
  y;
 
let () = i |> main >> o;
