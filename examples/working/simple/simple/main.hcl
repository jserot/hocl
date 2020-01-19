-- A minimalistic program

-- type int;

-- #pragma code("inp", "include/input.h", "input", "inputInit")
-- #pragma code("outp", "include/output.h", "output", "outputInit")
-- #pragma code("foo", "include/foo.h", "foo")

actor inp in () out(o: int);
actor foo in (i: int) out (o: int);
actor outp in (i: int) out ();

graph top_s in () out ()
struct
  wire w1: int
  wire w2: int
  node n1: inp()(w1)
  node n2: foo(w1)(w2)
  node n3: outp(w2)()
end;

graph top_f in () out ()
fun
  -- val () = inp |> foo >> outp
  val () = outp (foo (inp ()))
end;
