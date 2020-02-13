-- These examples show how tuples and bundles can be mixed in network expressions

type t;
type t';

node f in (i:t) out (o: t');

graph test1 in (i1: t, i2:t, i3:t) out (o1: t', o2:t', o3: t')
fun
 val o = map f [i1,i2,i3] -- o: t' bundle
 val o1 = o[0]
 val o2 = o[1]
 val o3 = o[2]
end;

graph test2 in (i1: t, i2:t, i3:t) out (o1: t', o2:t', o3: t')
fun
 val o = map f (i1,i2,i3) -- implicit tuple to bundle conversion
 val o1 = o[0]
 val o2 = o[1]
 val o3 = o[2]
end;

graph test3 in (i1: t, i2:t, i3:t) out (o1: t', o2:t', o3: t')
fun
 val [o1,o2,o3] = map f [i1,i2,i3] -- parallel bundle binding 
end;

graph test4 in (i1: t, i2:t, i3:t) out (o1: t', o2:t', o3: t')
fun
 val [o1,o2,o3] = map f (i1,i2,i3)
end;

graph test5 in (i1: t, i2:t, i3:t) out (o1: t', o2:t', o3: t')
fun
 val (o1,o2,o3) = map f [i1,i2,i3] -- implicit bundle to tuple conversion
end;

graph test6 in (i1: t, i2:t, i3:t) out (o1: t', o2:t', o3: t')
fun
 val (o1,o2,o3) = map f (i1,i2,i3) -- implicit tuple to bundle, then bundle to tuple conversion
end;
