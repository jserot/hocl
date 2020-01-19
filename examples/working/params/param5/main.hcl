-- Several examples of parameter usage

type t;

actor foo param (k: int) in (i: t) out (o: t);
actor bar param (k1: int, k2: bool) in (i: t) out (o: t);

graph top_s
  param (u3: int, u4:int, u5: int, u6: bool)
  in (i1: t, i2: t, i3:t, i4:t, i5: t, i6: t)
  out (o1: t, o2:t, o3: t, o4:t, o5: t, o6: t)
struct
  node n1: foo<2>(i1)(o1)
  node n2: foo<1+2>(i2)(o2)
  node n3: foo<u3>(i3)(o3)
  node n4: foo<u4*2>(i4)(o4)
  node n5: bar<u5/4,true>(i5)(o5)
  node n6: bar<4*2+1,u6>(i6)(o6)
end;

graph top_f
  param (u3: int, u4:int, u5: int, u6: bool)
  in (i1: t, i2: t, i3:t, i4:t, i5: t, i6: t)
  out (o1: t, o2:t, o3: t, o4:t, o5: t, o6: t)
fun
  val o1 = foo<2> i1    -- first case: local parameter
  val o2 = foo<1+2> i2  -- second case: local parameter expression (statically resolved)
  val o3 = foo<u3> i3    -- third case: input parameter
  val o4 = foo<u4*2> i4  -- fourth case: dependency on input parameter
  val o5 = bar<u5/4,true> i5 
  val o6 = bar<4*2+1,u6> i6 
end;
