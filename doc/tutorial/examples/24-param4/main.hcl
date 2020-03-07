-- A simple example illustrating parameter dependencies

node mult param (k: int) in (i: int) out (o: int);

node sub param (k: int) in (i: int) out (o: int)
fun
  val o = i |> mult<k+1>
end;

graph top param (k: int = 2) in (i: int) out (o: int)
fun
  val o = i |> sub<k>
end;
