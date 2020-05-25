#display;
input i: int;
output o: int;
node foo in (i: int) out (o: int);
val ( |> ) x f = f x;
val o =  i |> foo |> foo;
