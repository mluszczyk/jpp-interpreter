for x in good/unit/*;
do
  echo $x ;
  ./interpreter -t $x;
  ./interpreter -d $x;
  echo;
done;

