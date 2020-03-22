-- A simple example illustrating how to define top-level parameters

node mult param (k: int) in (i: int) out (o: int);

node sub param (k: int) in (i: int) out (o: int)
fun
  val o = i |> mult<k> |> mult<k>
end;

graph top param (k: int = 2) in (i: int) out (o: int)
fun
  val o = i |> sub<k>
end;
