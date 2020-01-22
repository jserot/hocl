type t;

actor f in (i: t) out ();

graph top in (i: t) out ()
fun
  val o = i |> repl 3 |> map f
end;
