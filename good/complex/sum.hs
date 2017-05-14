-- example of case expression


data List a = Cons (a) (List a) | Nil ;

sum :: (List Integer) -> Integer ;
sum t = case t of {
  Nil -> 0 ;
  Cons (a) (rest) -> a + (sum rest)
};

main = sum (Cons 1 (Cons 2 (Cons 3 (Cons 4 Nil))));
