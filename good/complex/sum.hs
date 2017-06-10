-- example of case expression

sum :: (List Integer) -> Integer ;
sum t = case t of {
  Nil -> 0 ;
  Cons (a) (rest) -> a + (sum rest)
};

main = sum (Cons 1 (Cons 2 (Cons 3 (Cons 4 Nil))));
