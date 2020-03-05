-- A simple graph with a simple, pure feedback

node foo in (i1: int, i2: int) out (o1: int, o2: int);

graph top_ok in (i: int) out (o: int)
fun
  val o = let rec (y,z) = foo (i,z) in y  -- TO FIX ! This works ...
  -- val cyc f x =                        -- But not this !
  --   let rec (y,z) = f (x,z) in
  --   y
  -- val o = cyc foo i
end;

graph top_bug in (i: int) out (o: int)  -- TO FIX !!!!!!
fun
  val cyc f x =                       
    let rec (y,z) = f (x,z) in
    y
  val o = cyc foo i
end;
