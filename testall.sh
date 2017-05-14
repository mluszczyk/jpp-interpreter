for x in good/*/*;
do
  echo $x ;
  ./interpreter -t $x;
  ./interpreter -d $x;
  echo;
done;

