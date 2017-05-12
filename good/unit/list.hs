data MyList a = MyCons a (MyList a) | MyNil;

main = MyCons 1 MyNil
