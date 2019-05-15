-- How to use tuples with multi inputs and multi outputs actors

type int;

actor f in () out (o1: int, o2: int);
actor g in (i: int) out (o: int);
actor h in (i1: int, i2: int) out ();

net (x,y) = f ();
net () = h (g x, g y);
