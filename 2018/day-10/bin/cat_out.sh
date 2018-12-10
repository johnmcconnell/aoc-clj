for f in resources/outputs/vectors.vectors.*.edn.out
do
  echo ">>>>> $f"
  cat $f
done
