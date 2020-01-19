type t;

val rec pipe fs x = match fs with -- TO BE MOVED TO prelude !
  [] -> x
| f::fs' -> pipe fs' (f x);

actor f1 in (i: t) out (o: t);
actor f2 in (i: t) out (o: t);
actor f3 in (i: t) out (o: t);

graph top in (i: t) out (o: t)
fun
  -- val o = i >> pipe [f1,f2,f3]
  val o = pipe [f1,f2,f3] i
end;
