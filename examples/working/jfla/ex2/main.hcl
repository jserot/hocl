type t;
  
node f in (i:t) out (o1:t, o2:t);
node k in (i:t) out (o:t);
node h in (i1:t, i2:t) out (o:t);

node g in (i: t) out (o: t)
fun
  val o = i |> k |> k
end;

graph top in (i: t) out (o: t)
fun
  val (x1,x2) = f i
  val o = h (g x1) (g x2)
end;
