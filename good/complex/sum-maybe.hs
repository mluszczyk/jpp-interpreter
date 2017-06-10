id n = n ;

sum :: (List Integer) -> Integer;
sum = foldl (\a -> \b -> a + b) 0 ;

sumMaybe :: (List (Maybe Integer)) -> Integer;
sumMaybe ml = sum (map (maybe 0 id) ml);


main = sumMaybe (Cons (Just 1) (
                 Cons (Just 2) (
                 Cons Nothing (
                 Cons (Just 3)
                 Nil))))
