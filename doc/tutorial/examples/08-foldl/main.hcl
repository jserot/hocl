type t;

node f in (i: t) out (o: t);
node g in (i1: t, i2: t) out (o: t);
node i in () out (o: t);
node z in () out (o: t);
node o in (i: t) out ();

graph top in (i: t) out (o: t)
fun
  val xs = i|> mapf (repl 4 f)
  val o = xs |> foldl g (z())
end;
