-- Example with an actor taking two single, local parameters

type t;

node foo param (k: int, l:int) in (i: t) out (o: t);

graph top_s in (i: t) out (o: t)
struct
  node n: foo<1,2>(i)(o)
end;

graph top_f in (i: t) out (o: t)
fun
  val o = i |> foo<1,2>
end;

-- #pragma code("inp", "include/input.h", "input", "inputInit")
-- #pragma code("outp", "include/output.h", "output", "outputInit")
-- #pragma code("foo", "include/foo.h", "foo")

