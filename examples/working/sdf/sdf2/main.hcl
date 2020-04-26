-- How to define actors with parametric CP-rates
-- The [foo] actor here consumes [n] tokens and produces 1 token per iteration

node foo param (k: int) in (i: int[k]) out (o: int)
actor
  systemc(loop_fn="foo", incl_file="./include/foo.h", src_file="./src/foo.cpp")
end;

graph top param (n: int = 3) in (i: int) out (o: int)
fun
  val o = i |> foo n
end;
