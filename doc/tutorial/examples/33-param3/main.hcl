-- A simple example illustrating how to define top-level parameters

node mult param (k: int) in (i: int) out (o: int);

node sub param (k: int) in (i: int) out (o: int)
fun
  val o = i |> mult<k> |> mult<k+1>
end;

graph top in (i: int) out (o: int)
fun
  val o = i |> sub<2>
end;
