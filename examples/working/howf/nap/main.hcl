type t;

node f in (i: t) out (o: t);

val nap n f x = mapf (repl n f) x;

graph top in (i: t) out (o1: t, o2:t, o3: t)
fun
  val [o1;o2;o3] = i |> nap 3 f
end;
