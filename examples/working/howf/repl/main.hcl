type t;

actor i in () out (o: t);
actor f in (i: t) out ();
actor o in (i: t) out ();

graph top in (i: t) out (o1: t, o2:t, o3:t)
fun
  val o = i |> repl 3
  val o1 = o[0]
  val o2 = o[1]
  val o3 = o[2]
  -- val o4 = o[3] -- Raises "invalid list index"
end;
