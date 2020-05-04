-- A simple program showing a hierarchical graph description
-- The [sub] node included in the [top] toplevel graph  is itself a graph, here described functionally
-- The [sub] node here uses a transfered parameter

node foo in (k: int param, i: int) out (o: int)
actor
  systemc(loop_fn="foo", incl_file="./include/foo.h", src_file="./src/foo.cpp")
end;

node bar in (i: int) out (o: int)
actor
  systemc(loop_fn="bar", incl_file="./include/bar.h", src_file="./src/bar.cpp")
end;

node sub in (k: int param, i: int) out (o: int)
fun
  val o = i |> foo k |> bar
end;

graph top in (i: int) out (o: int)
fun
  val o = sub 2 i
end;
