type t;

node i in () out (o: t);
node f in (i: t) out ();
node o in (i: t) out ();

graph top in (i: t) out (o1: t, o2:t, o3:t)
fun
  val o = i |> repl 3
  val o1 = o[0]
  val o2 = o[1]
  val o3 = o[2]
  -- val o4 = o[3] -- Raises "invalid list index"
end;
