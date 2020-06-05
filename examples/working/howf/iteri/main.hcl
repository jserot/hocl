-- The following example attach constants 0, 1, 2, ... to the
-- parameter input of each node instance `f`..

type t;

node f in (k: int param, i: t) out (o: t);

val rec iteri n f x =
  if n=0 then x
  else iteri (n-1) f (f 'n' x)
;

graph top in (i: t) out (o: t)
fun
  val o = iteri 3 f i
end;
