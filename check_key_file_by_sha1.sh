#/bin/bash


file_list=(rand.erl crypto.erl rand_algorithm.erl)

for file in "${file_list[@]}";do
  filePath=$(find . -name $file)
   echo $(sha1sum  $filePath)
done
