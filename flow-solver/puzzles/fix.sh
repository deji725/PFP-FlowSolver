for file in ./*.txt
do
  cat "$file" | sed "s/\./0/g" | tr '[:upper:]' '[:lower:]' > "$file".new
  mv "$file".new "$file"
done

