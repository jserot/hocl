-- A simple example with an actor taking a single, local parameter

node mult in (k: int param, i: int data) out (o: int data)
actor
  systemc(loop_fn="mult", incl_file="../include/mult.h", src_file="../src/mult.cpp")
end;

graph top in (i: int data) out (o: int data)
fun
  val o = mult ('2',i)
end;
