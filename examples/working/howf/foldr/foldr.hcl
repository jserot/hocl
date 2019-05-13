-- The [foldr] higher-order wiring function
-- It has type [('a * 'b -> 'b) -> 'a list -> 'b -> 'b]
-- and can be defined as : [foldr f z [x1, ..., xn] = f (x1, ... (f (xn,z), xn-1), ...)]
  
net rec foldr f xs z =
  match xs with
    [] -> z
  | x::xs' -> f (x, foldr f xs' z)
;
