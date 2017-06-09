data BinTree a = BinNode a (BinTree a) (BinTree a) | BinLeaf;

foldBinTree :: (b -> a -> b) -> b -> (BinTree a) -> b;
foldBinTree func val tree = case tree of {
  BinLeaf -> val;
  BinNode a left right ->
    foldBinTree func (
      func
        (foldBinTree func val left)
        a
    ) right
};

sum a b = a + b ;

main = foldBinTree (\list -> \val -> Cons val list) Nil
  (BinNode 4
    (BinNode 1 BinLeaf BinLeaf)
    (BinNode 8 (BinNode 6 BinLeaf BinLeaf) BinLeaf));

