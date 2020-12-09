node f in (i: int) out (o: int);
node qmf in (i: int) out (o1: int, o2: int);
node qmf2 in (i1: int, i2: int) out (o: int);

graph top in (i: int) out (o: int)
fun
  val rec fb d x =
    if d=0 then
      f x
    else
      let x1,x2 = qmf x in
      qmf2 (f x1) (fb (d-1) x2)
  val o = i |> fb 3
end;
