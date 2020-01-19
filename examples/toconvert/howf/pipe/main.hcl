type t;

actor f1 in (i: t) out (o: t);
actor f2 in (i: t) out (o: t);
actor f3 in (i: t) out (o: t);

graph top in (i: t) out (o: t)
fun
  val rec pipe fs x = match fs with [] -> x | f::fs' -> pipe fs' (f x) -- TO BE MOVED TO prelude !
  val o = i >> pipe [f1,f2,f3] i
end;
