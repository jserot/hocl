type t;

actor f in (i: t) out (o: t);
actor qmf in (i: t) out (o1: t, o2: t);
actor qmf' in (i1: t, i2: t) out (o: t);
actor i in () out (o: t);
actor o in (i: t) out ();

let rec fb d x =
  if d=0 then
    f x
  else
    let x1,x2 = qmf x in
    qmf' (f x1, fb (d-1) x2);

let () = i |> fb 3 >> o;
