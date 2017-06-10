sum t = case t of {
  Nil -> 0 ;
  Cons a -> a + (sum rest) -- too few parameters to the constructor
} ;

main = sum (Cons 1 (Cons 2 (Cons 3 (Cons 4 Nil))))
