type t;
type t';

actor i in () out (o: t);
actor f in (i: t) out (o: t');
actor g in (i: t') out ();

let _ = i |> repl 3 >> map f >> map g;
