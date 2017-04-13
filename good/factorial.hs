f :: Int -> Int ;
f x = (if x /= 0 then x * (f (x - 1)) else 1 );

main = f 5
