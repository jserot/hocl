-- Dependent parameters

node mult in (k: int param, i: int data) out (o: int data)
actor
  systemc(loop_fn="mult", incl_file="./include/mult.h", src_file="./src/mult.cpp")
end;

graph top in (k: int param = 1, i: int data) out (o: int data)
fun
  val o = mult ('k+1',i)
end;

