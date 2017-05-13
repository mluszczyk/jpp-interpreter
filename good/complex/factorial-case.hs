f x = case x /= 0 of {
  True -> x * (f (x - 1));
  False -> 1;
};

main = f 5
