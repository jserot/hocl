-- A rewriting of [../systemc1] with IO performed by explicit actors

node inp in () out (o: int)
actor
  systemc(init_fn="inputInit", loop_fn="input", incl_file="./include/input.h", src_file="./src/input.cpp")
end;

node foo in (i: int) out (o: int)
actor
  systemc(loop_fn="foo", incl_file="./include/foo.h", src_file="./src/foo.cpp")
end;

node outp in (i: int) out ()
actor
  systemc(init_fn="outputInit", loop_fn="output", incl_file="./include/output.h", src_file="./src/output.cpp")
end;

graph top in () out ()
fun
  val _ = inp |-> foo |> foo |> outp
end;
