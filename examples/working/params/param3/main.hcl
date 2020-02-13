-- Dependent parameters

type t;

node foo param (k: int) in (i: t) out (o: t);

graph top_s param (k: int = 1) in (i: t) out (o: t)
struct
  node n: foo<k+1>(i)(o)
end;

graph top_f param (k: int = 1) in (i: t) out (o: t)
fun
  val o = i |> foo<k+1>
end;

-- #pragma code("inp", "include/input.h", "input", "inputInit")
-- #pragma code("outp", "include/output.h", "output", "outputInit")
-- #pragma code("foo", "include/foo.h", "foo")

