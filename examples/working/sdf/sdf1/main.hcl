-- How to define multi-rate actors
-- The [foo] actor here consumes 8 tokens and produces 1 token per iteration

-- #pragma code("inp", "include/input.h", "input", "inputInit")
-- #pragma code("outp", "include/output.h", "output", "outputInit")
-- #pragma code("foo", "include/foo.h", "foo")

node foo in (i: int[8]) out (o: int);

graph top in (i: int[8]) out (o: int)
fun
  val o = i |> foo
end;
