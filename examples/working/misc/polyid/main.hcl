-- This example shows how to define polymorphic actors
-- The notation "$t" means "any type" and can be instanciated with any given type 
-- The interpretation of these actors is backend-dependent

node ident in (i: $t) out (o: $t);

graph top in (i1: int, i2: bool) out (o1: int, o2: bool)
fun
  val o1 = ident i1
  val o2 = ident i2
end;
