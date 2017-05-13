
data Wrapper1 a = W1 a;

data Wrapper2 a = W2 (Wrapper1 a) | Nil;

main = W2
