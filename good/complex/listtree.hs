sum a b = a + b ;


data ListTree a = LTNode a (List (ListTree a));

foldListTree :: (b -> (a -> b)) -> (b -> ((ListTree a) -> b));
foldListTree func val tree = case tree of {
  LTNode a children -> foldl (foldListTree func) (func val a) children
};

listTree = LTNode 3 (
  Cons (LTNode 1 (
      Cons (LTNode 2 Nil) (
      Cons (LTNode 3 (
        Cons (LTNode 4 Nil)
        Nil))
      Nil)))
  Nil);

main = foldListTree sum 0 listTree;
