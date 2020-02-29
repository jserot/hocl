-- Refinement of [../../simple/simple] for the SystemC backend

node foo in (i: int) out (o: int)
actor
  systemc(loop_fn="foo", incl_file="./include/foo.h", src_file="./src/foo.cpp")
end;

graph top in (i: int) out (o: int)
fun
  val o = i |> foo |> foo
end;
