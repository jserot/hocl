type t;
type t';

actor f1 in (i: t) out (t: t');
actor f2 in (i: t) out (t: t');
actor f3 in (i: t) out (t: t');
actor g in (i: t') out ();
actor i in () out (o: t);
actor o1 in (i: t') out ();
actor o2 in (i: t') out ();
actor o3 in (i: t') out ();

net xs = mapf [f1,f2,f3] (i ());
net ys = shuffle [2,1,0] xs;
net () = o1 (ys[0]);
net () = o2 (ys[1]);
net () = o3 (ys[2]);
