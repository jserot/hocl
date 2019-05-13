-- The [miter] higher-order function is a variant of [iter] where the results of
-- the intermediate applications are output
-- It has type [nat -> ('a -> 'a) -> 'a -> 'a list]
-- and can be defined as : [miter n f x = [f x, f (f x), ..., f^n x]]

net rec miter n f x =
  if n=0 then [] 
  else let y = f x in y :: (miter (n-1) f y)
;
