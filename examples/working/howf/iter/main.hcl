type t;

node f in (i: t) out (o: t);

graph top in (i: t) out (o: t)
fun
  val o = i |> iter 3 f
end;
