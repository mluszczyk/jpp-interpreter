(f 5) where {
  f x = (if x then x * (f (x - 1)) else 1 )
}
