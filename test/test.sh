#!/bin/sh
#echo "Testing files"

SEARCH_FOLDER="code_examples/*"

for f in $SEARCH_FOLDER
do
	echo "Evaluating " $f
    for ff in $f/*
    do    
    	echo "Analyzing $ff"  
        eval "./jsh analyze $ff"
    done
done