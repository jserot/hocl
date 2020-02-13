-- How to define actors with parametric CP-rates
-- The [foo] actor here consumes [n] tokens and produces 1 token per iteration

-- #pragma code("inp", "include/input.h", "input", "inputInit")
-- #pragma code("outp", "include/output.h", "output", "outputInit")
-- #pragma code("foo", "include/foo.h", "foo")

node inp param (k: int) in () out(o: int[k]);
node foo param (k: int) in (i: int[k]) out (o: int);
node outp in (i: int) out ();

graph main param (n: int = 8) in () out ()
fun
  val o = () |> inp<n> |> foo<n> |> outp
end;
