-- A simple example with an actor taking a single, local parameter
-- With three, equivalent formulations : postfix application, with the reverse application operator
-- and using partial application to generate a specialized version of the [mult] node.
-- Note the "'" around the parameter value: w/o them, the program does not type check
-- because parameters and scalars are distinct (the former translate into wire in the described
-- dataflow graph, the latter no)

node mult in (k: int param, i: int) out (o: int)
actor
  systemc(loop_fn="mult", incl_file="../include/mult.h", src_file="../src/mult.cpp")
end;

graph top1 in (i: int) out (o: int)
fun
  val o = mult '2' i
end;

graph top2 in (i: int) out (o: int)
fun
  val o = i |> mult '2'  -- Another, equivalent formulation
end;

graph top3 in (i: int) out (o: int)
fun
  -- Yet another formulation, using partial application
  val double = mult '2'  
  val o = i |> double
end;
