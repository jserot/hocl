type t;
type t';

node f1 in (i: t) out (o: t');
node f2 in (i: t) out (o: t');
node f3 in (i: t) out (o: t');

graph top in (i: t) out (o1: t', o2: t', o3: t')
fun
  val o = i |> mapf [f1;f2;f3]
  val o1 = o[0]
  val o2 = o[1]
  val o3 = o[2]
end;
