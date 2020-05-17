-- Several examples of parameter usage, in both structural and functional style

type t;

node foo in (k: int param, i: t data) out (o: t data);
node bar in (k1: int param, k2: bool param, i: t data) out (o: t data);

graph top_s
  in (u3: int param=1, u4:int param=1, u5: int param=1, u6: bool param=true,
      i1: t data, i2: t data, i3: t data, i4: t data, i5: t data, i6: t data)
  out (o1: t data, o2: t data, o3: t data, o4: t data, o5: t data, o6: t data)
struct
  box n1: foo ('2',i1)(o1)
  box n2: foo ('1+2',i2)(o2)
  box n3: foo (u3,i3)(o3)
  box n4: foo ('u4*2',i4)(o4)
  box n5: bar ('u5/4','true',i5)(o5)
  box n6: bar ('4*2+1',u6,i6)(o6)
end;

graph top_f
  in (u3: int param=1, u4:int param=1, u5: int param=1, u6: bool param=true,
      i1: t data, i2: t data, i3:t data, i4:t data, i5: t data, i6: t data)
  out (o1: t data, o2: t data, o3: t data, o4: t data, o5: t data, o6: t data)
fun
  val o1 = foo '2' i1       -- first case: local parameter
  val o2 = foo '1+2' i2   -- second case: local parameter expression
  val o3 = foo u3 i3    -- third case: input parameter
  val o4 = foo 'u4*2' i4  -- fourth case: dependency on input parameter
  val o5 = bar 'u5/4' 'true' i5 
  val o6 = bar '4*2+1' u6 i6 
end;
