-- The [foo] actor here consumes twice less tokens per iteration than produced by the
-- [inp] actor

node inp in () out (o: int[2])
actor
  systemc(loop_fn="inp", init_fn="inpInit", incl_file="./include/inp.h", src_file="./src/inp.cpp")
  preesm(loop_fn="inp", init_fn="inpInit", incl_file="./include/inp.h", src_file="./src/inp.cpp")
end;

node foo in (i: int[1]) out (o: int[1])
actor
  systemc(loop_fn="foo", incl_file="./include/foo.h", src_file="./src/foo.cpp")
  preesm(loop_fn="foo", incl_file="./include/foo.h", src_file="./src/foo.cpp")
end;

node outp in (i: int[2]) out ()
actor
  systemc(loop_fn="outp", incl_file="./include/outp.h", src_file="./src/outp.cpp")
  preesm(loop_fn="outp", incl_file="./include/outp.h", src_file="./src/outp.cpp")
end;

graph top in () out ()
fun
  val _ = inp |-> foo |> outp
end;
