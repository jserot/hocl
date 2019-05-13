-- The [repl] higher-order function
-- It has type [nat -> 'a -> 'a list]
-- and can be defined as : [repl n x = [x, ..., x]]
--                                      \---v---/
--                                       n times

net rec repl n x =
  if n=0 then []
  else x :: repl (n-1) x;
