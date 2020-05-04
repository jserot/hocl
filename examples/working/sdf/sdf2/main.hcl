-- How to define actors with parametric CP-rates
-- The [foo] actor here consumes [n] tokens and produces 1 token per iteration

node foo in (k: int param, i: int[k]) out (o: int)
actor
  systemc(loop_fn="foo", incl_file="./include/foo.h", src_file="./src/foo.cpp")
end;

graph top in (n:int param=3, i: int) out (o: int)
fun
  val o = foo(n,i)
end;
