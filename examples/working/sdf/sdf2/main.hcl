-- How to define actors with parametric CP-rates
-- The [foo] actor here consumes [n] tokens and produces 1 token per iteration

-- #pragma code("inp", "include/input.h", "input", "inputInit")
-- #pragma code("outp", "include/output.h", "output", "outputInit")
-- #pragma code("foo", "include/foo.h", "foo")

-- parameter n: int = 8;

actor inp param (k: int) in () out(o: int[k]);
actor foo param (k: int) in (i: int[k]) out (o: int);
actor outp in (i: int) out ();

graph main param (n: int) in () out (o: int) -- TODO : allow default value 8 for param n here ?
fun
  val o = inp<n> |-> foo<n>
end;

graph top in () out (o: int)
fun
  val o = main<8> ()
end;
