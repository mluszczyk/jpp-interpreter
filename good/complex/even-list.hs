data EvenList a = EvenNil | EvenCons a (OddList a);
data OddList a = OddCons a (EvenList a);

oddLength :: (OddList a) -> Integer;
evenLength evenList = case evenList of {
  EvenNil -> 0;
  EvenCons _ rest -> 1 + oddLength rest;
};

oddLength oddList = case oddList of {
  OddCons _ rest -> 1 + evenLength rest
};


main = EvenCons 1 (OddCons 2 (EvenCons 3 (OddCons 4 EvenNil)));
