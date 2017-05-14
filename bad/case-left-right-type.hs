-- mismatch in types (case pattern versus result)

data Wrapper1 a = W1 a;
data Wrapper2 a = W2 (Wrapper1 a);

main x = case x of {
  W2 a -> a + 5 ;
}
