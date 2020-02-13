type t;

node f in (i: t) out (o: t);

graph top in (i: t) out (o1: t, o2: t, o3:t)
fun
  val o = i |> miter 3 f
  val o1 = o[0]
  val o2 = o[1]
  val o3 = o[2]
end;
