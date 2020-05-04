type t1;
type t2;

node f1 in (i: t1) out (o: t2);
node f2 in (i: t1) out (o: t2);
node f3 in (i: t1) out (o: t2);

graph top in (i: t1) out (o1: t2, o2: t2, o3: t2)
fun
  val [o1;o2;o3] = i |> mapf [f1;f2;f3] |> shuffle [2;1;0]
end;
