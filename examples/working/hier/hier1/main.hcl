-- A simple program showing a hierarchical graph description
-- The [sub_s] node included in the [top1] (resp. [top2]) toplevel graph 
-- is itself a graph, described structurally (resp. functionally)

type t;

node foo in (i: t) out (o: t);
node bar in (e: t) out (s: t);

node sub_s in (i: t) out (o: t)
struct
  wire w: t
  node n1: foo(i)(w)
  node n2: bar(w)(o)
end;

node sub_f in (i: t) out (o: t)
fun
  val o = bar (foo i)
end;

graph top1 in (i: t) out (o: t)
fun
  val o = sub_s i 
end;

graph top2 in (i: t) out (o: t)
fun
  val o = sub_f i
end;
