type t1;
type t2;

node f1 in (i: t1) out (o: t2);
node f2 in (i: t1) out (o: t2);
node f3 in (i: t1) out (o: t2);

graph top in (i1: t1, i2: t1, i3: t1) out (o1: t2, o2: t2, o3: t2)
fun
 val [o1;o2;o3] = [i1;i2;i3] |> map2f [f1;f2;f3]
end;
