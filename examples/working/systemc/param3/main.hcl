-- Dependent parameters

node mult param (k: int) in (i: int) out (o: int)
actor
  systemc(loop_fn="mult", incl_file="./include/mult.h", src_file="./src/mult.cpp")
end;

graph top param (k: int = 1) in (i: int) out (o: int)
fun
  val o = mult<k+1> i
end;

