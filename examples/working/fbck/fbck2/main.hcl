-- Describing cyclic wiring (feedback) - ex2

type t;
type t';

actor foo in (i1: t, i2: t') out (o1: t, o2: t'); 
actor bar in (i: t') out (o: t'); 
actor biz in (i: t') out (o: t'); 

graph top in (i: t) out (o: t)
fun
  val cyc f h x =                  -- this howf introduces a cycle
    let rec (y,z) = f (x, h z) in
    y
  val o = i |> cyc foo (biz @@ bar)
end;
