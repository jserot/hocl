type t;
type t';

actor f in (i1: t, i2: t') out (o: t);
actor g in (i: t') out (o: t');
actor i1 in () out (o: t);
actor i2 in () out (o: t');
actor o in (i: t) out ();

net double n =  n*2;

net rec chain n f g z x =
  if n=0 then x
  else
    let z' = iter (double n) g z in
    let x' = f (x,z') in
    chain (n-1) f g z x';

net () = i1 |> chain 3 f g (i2()) >> o;
