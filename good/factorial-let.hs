let f = \x -> (if x then (x * (f (x - 1))) else 1) in (f 5)
