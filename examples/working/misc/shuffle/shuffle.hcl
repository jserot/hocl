-- The [shuffle] wiring function
-- It has type [nat list -> 'a list -> ' a list]
-- and can be defined as : [shuffle [k1, ..., kn]  [x1, ..., xn] = [x_{k1}, ..., x_{kn}]]

net rec shuffle ks xs = match ks with
    [] -> []
  | k::ks' -> xs[k] :: shuffle ks' xs
;
