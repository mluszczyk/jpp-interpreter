data List a = Cons a (List a) | Nil ;

main = (Cons 1 (Cons 2 (Cons 3 Nil))) == (Cons 1 (Cons 2 (Cons 3 Nil)))
