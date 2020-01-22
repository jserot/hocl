type t;
type t';

actor f in (i1: t, i2: t') out (o: t);
actor g in (i: t') out (o: t');
actor z in () out (o: t');

graph top in (i: t) out (o: t)
fun
  val double n =  n*2
  val rec chain n f g z x =
    if n=0 then x
    else
      let z' = iter (double n) g z in
      let x' = f (x,z') in
      chain (n-1) f g z x'
  val o = i |> chain 3 f g (z())
end;
