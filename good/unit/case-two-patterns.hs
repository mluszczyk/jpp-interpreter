data MyEither a b = MyLeft a | MyRight b ;

either lf rf e = case e of {
  MyLeft a -> lf a ;
  MyRight b -> rf b 
};

main = either
