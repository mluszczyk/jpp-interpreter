data List a = Cons a (List a) | Nil;

main = case a of {
  Cons x (Cons y (Cons z t)) -> x ;
  _ -> 0
} where {
  a = Cons 3 (Cons 1 (Cons 2 Nil))
}
