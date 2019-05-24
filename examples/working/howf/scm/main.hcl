-- The classical "split-compute-merge" skeleton

type t;
type t';

actor split in (i:t) out (o1:t, o2:t, o3:t, o4:t);
actor comp in (i:t) out (o:t');
actor merge in (i1:t', i2:t', i3:t', i4:t') out (o:t');
actor inp in () out (o:t);
actor outp in (i:t') out ();

let scm s c m x = m (map c (split x));

let _ = inp |> scm split comp merge >> outp;


