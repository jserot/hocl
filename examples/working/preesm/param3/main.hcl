-- Reformulation of [../../params/param2] with explicit IO actor to use the Preesm backend

node inp in () out (o: int)
actor
  preesm(init_fn="inputInit", loop_fn="input", incl_file="./include/input.h", src_file="./src/input.cpp")
end;

node outp in (i: int) out ()
actor
  preesm(loop_fn="output", incl_file="./include/output.h", src_file="./src/output.cpp")
end;

node mult param (k: int) in (i: int) out (o: int)
actor
  preesm(loop_fn="mult", incl_file="./include/mult.h", src_file="./src/mult.cpp")
end;

graph top param (k: int=1) in () out ()
fun
  val _ = inp |-> mult (k+1) |> outp
end;
