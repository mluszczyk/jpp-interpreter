(f 5) where {
  f x = (if x then x * (g (x - 1)) else 1 );
  g x = (if x then x * (f (x - 1)) else 1 );
}
