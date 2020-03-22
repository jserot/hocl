node f in (i: int) out (o1: int, o2: int);
node g in (i: int) out (o:int);
node h in (i1:int, i2:int) out (o: int);

-- Two levels, fnal
graph top2f in (i: int) out (o: int)
fun
  val diamond left middle right x = 
   let (x1,x2) = left x in
   right (middle x1, middle x2)
  val o =  diamond f (diamond f g h) h  i
end;

-- Two levels, structural
graph top2s in (i: int) out (o: int)
struct
  wire w1, w2, w3, w4, w5, w6: int
  wire w7, w8, w9, w10, w11, w12: int
  node f1: f(i)(w1,w2)
  node f2: f(w1)(w3,w4)
  node f3: f(w2)(w5,w6)
  node g1: g(w3)(w7)
  node g2: g(w4)(w8)
  node g3: g(w5)(w9)
  node g4: g(w6)(w10)
  node h1: h(w7,w8)(w11)
  node h2: h(w9,w10)(w12)
  node h3: h(w11,w12)(o)
end;

-- Three levels, fnal
graph top3f in (i: int) out (o: int)
fun
  val diamond left middle right x = 
   let (x1,x2) = left x in
   right (middle x1, middle x2)
  val o =  diamond f (diamond f (diamond f g h) h) h  i
end;

-- Three levels, structural
-- TBW 
