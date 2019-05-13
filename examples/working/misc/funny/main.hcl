type t;
type t';

actor f : t * t' -> t;
actor g : t' -> t';
actor i1 : unit -> t;
actor i2 : unit -> t';
actor o : t -> unit;

net double n =  n*2;

net rec chain n f g z x =
  if n=0 then x
  else
    let z' = iter (double n) g z in
    let x' = f (x,z') in
    chain (n-1) f g z x';

net () = o (chain 3 f g (i2 ()) (i1 ()));
