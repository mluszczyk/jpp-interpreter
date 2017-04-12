main = f 5;

f x = (if x /= 0 then x * (g (x - 1)) else 1 );
g x = f x
