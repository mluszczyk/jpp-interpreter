data Wrapper a = W a;

unwrap a = case a of {
  W bare -> bare
} ;

main = unwrap (W 5)


