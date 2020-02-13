-- The classical "split-compute-merge" pattern

type t;
type t';

node split in (i:t) out (o1:t, o2:t, o3:t, o4:t);
node comp in (i:t) out (o:t');
node merge in (i1:t', i2:t', i3:t', i4:t') out (o:t');

graph top in (i: t) out (o: t')
fun
  val scm s c m x = m (map c (split x))
  val o = i |> scm split comp merge
end;


