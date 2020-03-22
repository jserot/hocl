-- A simple example illustrating parameter passing btw hierarchy levels

node mult param (k: int) in (i: int) out (o: int);

node sub param (k: int) in (i: int) out (o: int)
fun
  val o = i |> mult<k> |> mult<k>
end;

graph top in (i: int) out (o: int)
fun
  val o = i |> sub<2>
end;
