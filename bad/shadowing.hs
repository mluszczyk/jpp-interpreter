-- name shadowing is considered bad style and thus illegal
-- it works in the less strict -d mode, though

x = 5;
f x = 5;

main = f 4
