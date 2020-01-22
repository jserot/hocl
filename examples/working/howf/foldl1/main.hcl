type t;

actor f in (i: t) out (o: t);
actor g in (i1: t, i2: t) out (o: t);
actor z in () out (o: t);

graph top in (i: t) out (o: t)
fun
  val o = i |> repl 5 |> foldl1 g
end;
