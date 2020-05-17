-- Reformulation of [../../params/param1] with explicit IO actor to use the Preesm backend

node inp in () out (o: int)
actor
  preesm(init_fn="inputInit", loop_fn="input", incl_file="./include/input.h", src_file="./src/input.cpp")
end;

node outp in (i: int) out ()
actor
  preesm(loop_fn="output", incl_file="./include/output.h", src_file="./src/output.cpp")
end;

node mult in (k: int param, i: int) out (o: int)
actor
  preesm(loop_fn="mult", incl_file="./include/mult.h", src_file="./src/mult.cpp")
end;

graph top in () out ()
fun
  val _ = inp |-> mult '2' |> outp
end;
