type t;

actor i in () out (o: t);
actor f in (i: t) out (o: t);
actor o in (i: t) out ();

let ys = i |> miter 3 f;
let _ = o (ys[0]);
let _ = o (ys[1]);
let _ = o (ys[2]);
