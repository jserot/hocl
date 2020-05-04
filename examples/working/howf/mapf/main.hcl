type t1;
type t2;

node f1 in (i: t1) out (o: t2);
node f2 in (i: t1) out (o: t2);
node f3 in (i: t1) out (o: t2);

graph top in (i: t1) out (o1: t2, o2: t2, o3: t2)
fun
  val o = i |> mapf [f1;f2;f3]
  val o1 = o[0]
  val o2 = o[1]
  val o3 = o[2]
end;
