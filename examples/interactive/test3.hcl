#display;
input i: int;
output o: int;
node foo in (i: int) out (o: int);
val rec iter n f x = if n=0 then x else iter (n-1) f (f x);
val o =  iter 4 foo i;
