type t;

node f in (i: t) out (o: t);
node qmf in (i: t) out (o1: t, o2: t);
node qmfp in (i1: t, i2: t) out (o: t);

graph top in (i: t) out (o: t)
fun
  val rec fb d x =
    if d=0 then
      f x
    else
      let x1,x2 = qmf x in
      qmfp (f x1) (fb (d-1) x2)
  val o = i |> fb 3
end;
