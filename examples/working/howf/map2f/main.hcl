type t;
type t';

node f1 in (i: t) out (o: t');
node f2 in (i: t) out (o: t');
node f3 in (i: t) out (o: t');

graph top in (i1: t, i2: t, i3: t) out (o1: t', o2:t', o3:t')
fun
 val [o1;o2;o3] = [i1;i2;i3] |> map2f [f1;f2;f3]
end;
