#!/bin/bash

count=0

for filename in $(ls tests)
do
    if  [[ $(echo $filename | cut -f 3 -d ".") == "out" ]]; then continue; fi

    temp=$(echo $filename | cut -f 1 -d ".") #filename without the extension
    ./hw3 < "tests/$temp.in" > "myOutput/$temp.out"

    echo "-------$filename-------"
    diff "tests/$temp.in.out" "myOutput/$temp.out"
    if  [[ $? == 0 ]]
    then
        echo "files are identical"
    else
        ((count++))
    fi
    printf "\n\n\n\n"
done

echo "There are ${count} diffs"