-- The [iter] higher-order function
-- It has type [nat -> ('a -> 'a) -> 'a -> 'a]
-- and can be defined as : [iter n f x = f^n x = f (... f (f x) ...)]
--                                               \--------v-------/
--                                                  n applications

net rec iter n f x =
  if n=0 then x
  else iter (n-1) f (f x);
