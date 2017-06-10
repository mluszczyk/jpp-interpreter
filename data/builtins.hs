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

hasBuiltins_ = True;

data List a = Cons a (List a) | Nil;

take :: Integer -> (List a) -> (List a);
take n l =
  if n == 0 then Nil else case l of {
    Nil -> Nil ;
    Cons a tail -> Cons a (take (n - 1) tail)
  } ;

length :: (List a) -> Integer;
length a = case a of {
	Nil -> 0;
  Cons _ tail -> 1 + length tail;
};


foldl :: (b -> (a -> b)) -> b -> (List a) -> b;
foldl func val list = case list of {
 Nil -> val;
 Cons a rest -> foldl func (func val a) rest;
};


map :: (a -> b) -> (List a) -> (List b);
map func list = case list of {
  Nil -> Nil;
  Cons a rest -> Cons (func a) (map func rest)
};

data Pair a b = Pair a b;

fst p = case p of {
  Pair a _ -> a
};

snd p = case p of {
  Pair _ b -> b
};


zip :: (List a) -> (List b) -> (List (Pair a b) );
zip a b = case (Pair a b) of {
  Pair Nil _ -> Nil;
  Pair _ Nil -> Nil;
  Pair (Cons ha ta) (Cons hb tb) -> Cons (Pair ha hb) (zip ta tb);
};

data Maybe a = Nothing | Just a;

maybe :: b -> (a -> b) -> (Maybe a) -> b;
maybe nothingVal justFunc val = case val of {
  Nothing -> nothingVal;
  Just a -> justFunc a
}
