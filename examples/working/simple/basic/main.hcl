node foo in (i: int) out (o: int);
-- No implementation given here -> [foo] will be viewed as a black box

graph top_s in (i: int) out (o: int)
struct
  wire w: int
  box n1: foo(i)(w)
  box n2: foo(w)(o)
end;

graph top_f in (i: int) out (o: int)
fun
  val o = i |> foo |> foo
end;
