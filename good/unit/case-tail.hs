tailE a = case a of {
	Nil -> Nil;
	Cons _ rest -> rest
};

main = tailE
