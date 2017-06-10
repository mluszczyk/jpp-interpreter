echo "Good"
for x in good/*/*;
do
  echo $x ;
  cat $x ;
  echo "--";
  ./interpreter -t $x || { echo error; exit 1; };
  ./interpreter -d $x || { echo error; exit 1; };
  echo;
  echo;
done;

echo;
echo;
echo "Bad"
for x in bad/*;
do
  echo $x ;
  cat $x ;
  echo "--";
  ./interpreter -t $x;
  ./interpreter -d $x;
  echo;
  echo;
done;
