-- A simple program showing a hierarchical graph description
-- The [sub_s] node included in the [top1] (resp. [top2]) toplevel graph 
-- is itself a graph, described structurally (resp. functionally)

node foo in (i: int) out (o: int)
actor
  systemc(loop_fn="foo", incl_file="./include/foo.h", src_file="./src/foo.cpp")
end;

node bar in (i: int) out (o: int)
actor
  systemc(loop_fn="bar", incl_file="./include/bar.h", src_file="./src/bar.cpp")
end;

node sub_s in (i: int) out (o: int)
struct
  wire w: int
  node n1: foo(i)(w)
  node n2: bar(w)(o)
end;

node sub_f in (i: int) out (o: int)
fun
  val o = i|> foo |> bar
end;

graph top1 in (i: int) out (o: int)
fun
  val o = sub_s i 
end;

graph top2 in (i: int) out (o: int)
fun
  val o = sub_f i
end;
