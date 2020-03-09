-- A simple application for exercising the Preesm backend
-- All IOs are here performed using actors (so that the top graph is IO-less)

node inp in () out (o: int)
actor
  preesm(init_fn="inputInit", loop_fn="input", incl_file="./include/input.h", src_file="./src/input.cpp")
end;

node foo in (i: int) out (o: int)
actor
  preesm(loop_fn="foo", incl_file="./include/foo.h", src_file="./src/foo.cpp")
end;

node outp in (i: int) out ()
actor
  preesm(loop_fn="output", incl_file="./include/output.h", src_file="./src/output.cpp")
end;

graph top in () out ()
fun
  val _ = inp |-> foo |> outp
end;
