-- A simple example with an actor taking a single, local parameter

node mult param (k: int) in (i: int) out (o: int)
actor
  systemc(loop_fn="mult", incl_file="./include/mult.h", src_file="./src/mult.cpp")
end;

graph top in (i: int) out (o: int)
fun
  val o = mult<2> i
end;
