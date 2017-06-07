data List a = Cons a (List a) | Nil;
tailE a = case a of {
	Nil -> Nil;
	Cons _ rest -> rest
};

main = tailE
