#!/bin/bash
for FILE in $(ls *.F90)
do    
    echo " $FILE ->  ${FILE%.*} + ${FILE#*.}"
    elmerf90 $FILE -o  ${FILE%.*}.so
done
	 
 
