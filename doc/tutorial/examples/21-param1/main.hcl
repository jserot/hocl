-- A simple example with an actor taking a parameter
-- The parameter value is here specified as a toplevel constant

node mult param (k: int) in (i: int) out(o: int);

graph top_s in (i: int) out (o: int)
struct
  node n: mult<2>(i)(o)
end;

graph top_f in (i: int) out (o: int)
fun
  val o = i |> mult<2>
end;
