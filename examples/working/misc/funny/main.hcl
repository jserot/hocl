type t1;
type t2;

node f in (i1: t1, i2: t2) out (o: t1);
node g in (i: t2) out (o: t2);
node z in () out (o: t2);

graph top in (i: t1) out (o: t1)
fun
  val double n =  n*2
  val rec chain n f g z x =
    if n=0 then x
    else
      let zz = iter (double n) g z in
      let xx = f (x,zz) in
      chain (n-1) f g z xx
  val o = i |> chain 3 f g (z())
end;
