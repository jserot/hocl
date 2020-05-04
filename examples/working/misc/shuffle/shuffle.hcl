-- The [shuffle] wiring function
-- It has type [nat list -> $t list -> $t list]
-- and can be defined as : [shuffle [k1, ..., kn]  [x1, ..., xn] = [x_{k1}, ..., x_{kn}]]
-- This should be in the standard prelude (?)

val rec shuffle ks xs = match ks with
    [] -> []
  | k::ks -> xs[k] :: shuffle ks xs
;
