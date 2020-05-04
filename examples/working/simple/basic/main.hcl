node foo in (i: int data) out (o: int data);
-- No implementation given here -> [foo] will be viewed as a black box

graph top_s in (i: int data) out (o: int data)
struct
  wire w: int data
  box n1: foo(i)(w)
  box n2: foo(w)(o)
end;

graph top_f in (i: int data) out (o: int data)
fun
  val o = i |> foo |> foo
end;
