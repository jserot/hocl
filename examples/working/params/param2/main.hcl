-- Example with an actor taking two single, local parameters

node mult in (k1: int param, k2: int param, i: int data) out (o: int data)
actor
  systemc(loop_fn="mult", incl_file="./include/mult.h", src_file="./src/mult.cpp")
end;

graph top in (i: int data) out (o: int data)
fun
  val o = mult ('2','1',i)
end;
