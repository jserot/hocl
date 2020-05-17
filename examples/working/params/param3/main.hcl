-- Dependent parameters

node mult in (k: int param, i: int) out (o: int)
actor
  systemc(loop_fn="mult", incl_file="./include/mult.h", src_file="./src/mult.cpp")
end;

graph top in (k: int param = 1, i: int) out (o: int)
fun
  val o = i |> mult 'k+1'
end;

