-- A variant of [../hier2b] in which the parameter is supplied externally

node foo param (k: int) in (i: int) out (o: int)
actor
  systemc(loop_fn="foo", incl_file="./include/foo.h", src_file="./src/foo.cpp")
end;

node bar in (i: int) out (o: int)
actor
  systemc(loop_fn="bar", incl_file="./include/bar.h", src_file="./src/bar.cpp")
end;

node sub param (k: int) in (i: int) out (o: int)
fun
  val o = i |> foo<k> |> bar
end;

graph top param (k: int = 2) in (i: int) out (o: int)
fun
  val o = sub<k> i
end;
