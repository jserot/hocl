-- A simple program showing a hierarchical graph description
-- The [sub_s] node included in the [top1] (resp. [top2]) toplevel graph 
-- is itself a graph, described structurally (resp. functionally)

type t;

node foo in (i: t) out (o: t);
node bar in (e: t) out (s: t);

node sub in (i: t) out (o: t)
fun
  val o = i |> foo |> bar
end;

graph top in (i: t) out (o: t)
fun
  val o = i|> sub
end;
