-- Describing cyclic wiring (feedback) - ex2

type t1;
type t2;

node foo in (i1: t1, i2: t2) out (o1: t1, o2: t2); 
node bar in (i: t2) out (o: t2); 
node biz in (i: t2) out (o: t2); 

graph top in (i: t1) out (o: t1)
fun
  val cyc f h x =                  -- this howf introduces a cycle
    let rec (y,z) = f (x, h z) in
    y
  val o = i |> cyc foo (biz @@ bar)
end;
