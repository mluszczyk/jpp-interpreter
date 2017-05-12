data List a = Cons a (List a) | Nil ;

data ListOfFunc a b = LOF (List (a -> b) ) ;

id = \x -> x ;

main = LOF (Cons id Nil)
