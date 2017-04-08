sum (Cons 1 (Cons 2 (Cons 3 (Cons 4 Nil)))) where {
  data List a = Cons (a) (List a) | Nil ;
  sum t = case t of {
    Nil -> 0 ;
    Cons (a) -> a + (sum rest)
  }
}
