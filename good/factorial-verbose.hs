main = f 5;

f x = (if x then x * (g (x - 1)) else 1 );
g x = (if x then x * (f (x - 1)) else 1 );
