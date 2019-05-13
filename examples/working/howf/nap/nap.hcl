-- The [nap] higher-order function
-- It has type [nat -> ('a -> 'b) -> 'b list]
-- and can be defined as : [nap n f x = [f x, ..., f x]]
--                                       \-----v-----/

net rec nap n f x =
  if n=0 then []
  else f x :: nap (n-1) f x
;
