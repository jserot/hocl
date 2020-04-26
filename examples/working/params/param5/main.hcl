-- Several examples of parameter usage, in both structural and functional style

type t;

node foo param (k: int) in (i: t) out (o: t);
node bar param (k1: int, k2: bool) in (i: t) out (o: t);

graph top_s
  param (u3: int=1, u4:int=1, u5: int=1, u6: bool=true)
  in (i1: t, i2: t, i3:t, i4:t, i5: t, i6: t)
  out (o1: t, o2:t, o3: t, o4:t, o5: t, o6: t)
struct
  box n1: foo [2] (i1)(o1)
  box n2: foo [1+2] (i2)(o2)
  box n3: foo [u3] (i3)(o3)
  box n4: foo [u4*2] (i4)(o4)
  box n5: bar [u5/4,true] (i5)(o5)
  box n6: bar [4*2+1,u6] (i6)(o6)
end;

graph top_f
  param (u3: int=1, u4:int=1, u5: int=1, u6: bool=true)
  in (i1: t, i2: t, i3:t, i4:t, i5: t, i6: t)
  out (o1: t, o2:t, o3: t, o4:t, o5: t, o6: t)
fun
  val o1 = i1 |> foo 2    -- first case: local parameter
  val o2 = i2 |> foo (1+2)   -- second case: local parameter expression (statically resolved)
  val o3 = i3 |> foo u3    -- third case: input parameter
  val o4 = i4 |> foo (u4*2)  -- fourth case: dependency on input parameter
  val o5 = i5 |> bar (u5/4,true) 
  val o6 = i6 |> bar (4*2+1,u6) 
end;
