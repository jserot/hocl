node f1 in (i: int) out(o: int);
node f2 in (i: int) out(o: int);
node f3 in (i: int) out(o: int);

graph top1 in (i: int) out (o1: int, o2: int, o3: int)
struct
  node n1: f1(i)(o1)
  node n2: f2(i)(o2)
  node n2: f3(i)(o3)
end;

graph top2 in (i: int) out (o1: int, o2: int, o3: int)
fun
  val (o1,o2,o3) = i |> mapf [f1,f2,f3]
end;

graph top3 in (i: int) out (o1: int, o2: int, o3: int)
fun
  val (o1,o2,o3) = i |> mapf [iter 2 f1, iter 3 f2, iter 4 f3]
end;
