node foo in (i: int) out (o: int);
-- No implementation given here -> will be viewed as a black box

graph top_s in (i: int) out (o: int)
struct
  wire w: int
  node n1: foo(i)(w)
  node n2: foo(w)(o)
end;

graph top_f in (i: int) out (o: int)
fun
  val o = i |> foo |> foo
end;
