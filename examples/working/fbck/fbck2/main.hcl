-- Describing cyclic wiring (feedback) - ex2

type t;
type t';

node foo in (i1: t, i2: t') out (o1: t, o2: t'); 
node bar in (i: t') out (o: t'); 
node biz in (i: t') out (o: t'); 

graph top in (i: t) out (o: t)
fun
  val cyc f h x =                  -- this howf introduces a cycle
    let rec (y,z) = f (x, h z) in
    y
  val o = i |> cyc foo (biz @@ bar)
end;
