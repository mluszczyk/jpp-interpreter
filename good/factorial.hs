f x = (if x then x * (f (x - 1)) else 1 );

main = f 5
