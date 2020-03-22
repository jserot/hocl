node i in () out (o: int[1]);
node f in (i: int[3]) out (o: int[2]);
node o in (i: int[1]) out ();

graph top in () out ()
fun
  val _ = i |-> f |> o
end;
