main = let {
  f :: Integer -> Integer;
  f = \x -> if x /= 0 then (x * (f (x - 1))) else 1
} in (f 5)
