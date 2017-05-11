data Bool = True | False;

if_ a b c = case a of {
  True -> b ;
  False -> c
};

and_ a b = case a of {
  True -> b ;
  False -> False
};

or_ a b = case a of {
  True -> True ;
  False -> b
};

hasBuiltins_ = True
