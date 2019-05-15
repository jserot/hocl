-- Recursive wiring - ex1

type int;

actor f in (i1: int, i2: int) out (o1: int, o2: int); 
actor i in () out (o: int);
actor o in (i1: int) out ();

net main x = 
  let rec (y,z) = f (x,z) in
  y;
 
net () = i |> main >> o;
