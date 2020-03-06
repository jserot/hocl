-- How to define multi-rate actors
-- The [foo] actor here consumes 4 tokens and produces 2 token per iteration

node foo in (i: int[4]) out (o: int[2])
actor
  systemc(loop_fn="foo", incl_file="./include/foo.h", src_file="./src/foo.cpp")
end;

graph top in (i: int[4]) out (o: int[2])
fun
  val o = i |> foo
end;
