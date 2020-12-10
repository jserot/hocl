node mult in (k: int param, i: int) out (o: int);

node sub
  in (k:int param, i:int) out (o:int)
fun
  val o = i |> mult k  |> mult 'k+1'
end;

graph top
  in (n:int param=2, i:int) out (o:int)
fun
  val o = i |> sub n
end;
