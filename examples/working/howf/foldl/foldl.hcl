-- The [foldl] higher-order wiring function
-- It has type [('a * 'b -> 'a) -> 'a -> 'b list -> 'a]
-- and can be defined as : [foldl f z [x1, ..., xn] = f (... (f (z,x1), x2), ..., xn)
  
net rec foldl f z xs =
  match xs with 
  [] -> z
| x::xs' -> foldl f (f (z,x)) xs'
;
