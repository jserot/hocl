-- Example with an actor taking two single, local parameters
-- Again, with two possible formulation, with and w/o the reverse app operator

node mult in (k1: int param, k2: int param, i: int) out (o: int)
actor
  systemc(loop_fn="mult", incl_file="./include/mult.h", src_file="./src/mult.cpp")
end;

graph top1 in (i: int) out (o: int)
fun
  val o = mult '2' '1' i
end;

graph top2 in (i: int) out (o: int)
fun
  val o = i |> mult '2' '1'
end;
