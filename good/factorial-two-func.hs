f x = (if x then x * (g (x - 1)) else 1 );
g = f;

main = f 5
