-- Parameters are implicitely broadcasted (IOW, the output port of a parameter box 
-- can be directly wired to several distinct input ports)
-- This interpretation is naturally supported by the Preesm backend
-- When using the SystemC backend, explicit bcast nodes are automatically inserted 
-- Inserted bcast nodes wan be viewed with the Dot backend by passing the [-insert_bcasts] option

type t;

actor foo param (k: int) in (i: t) out (o: t);
actor bar param (k: int) in (i: t) out (o: t);

graph top param (p: int) in (i: t) out (o: t)
fun
  -- val o  = i |> foo<p> |> bar<p>
  val o  = i |> foo<p>  -- TO FIX ! Syntax error
end;

-- #pragma code("inp", "include/input.h", "input", "inputInit")
-- #pragma code("outp", "include/output.h", "output", "outputInit")
-- #pragma code("foo", "include/foo.h", "foo")
-- #pragma code("bar", "include/bar.h", "bar")

