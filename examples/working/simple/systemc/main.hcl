-- Refinement of [../basic] for using the SystemC backend

node foo in (i: int data) out (o: int data)
actor
  systemc(loop_fn="foo", incl_file="./include/foo.h", src_file="./src/foo.cpp")
end;

graph top in (i: int data) out (o: int data)
fun
  val o = i |> foo |> foo
end;
