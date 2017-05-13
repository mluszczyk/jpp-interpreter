data Wrapper a = W a;

unwrap a = case a of {
  W a -> a
} ;

main = unwrap (W 5)


