type t;
type t';

actor f1 in (i: t) out (t: t');
actor f2 in (i: t) out (t: t');
actor f3 in (i: t) out (t: t');

graph top in (i: t) out (o1: t', o2: t', o3: t')
fun
  val [o1,o2,o3] = i |> mapf [f1,f2,f3] |> shuffle [2,1,0]
end;
