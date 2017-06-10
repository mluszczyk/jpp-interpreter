-- redefinition is illegal

main = case Nil of {
  Cons map _ -> 4
}
