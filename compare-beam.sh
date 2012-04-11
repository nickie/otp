#!/bin/sh

orig=./beam-vanilla
curr=./beam-nickie

if [ "$1" = "-v" ]; then
    for beam in `ls $orig`; do
        echo "$beam:"
        diff -a $orig/$beam $curr/$beam | grep -a '^[<>]' | grep -a -v '^[<>] debug_info'
        if [ $? -eq 0 ]; then
            echo "Difference in: $beam"
        fi
        echo "--------------------------------------------------"
    done
else
    for beam in `ls $orig`; do
        lines=`diff -a $orig/$beam $curr/$beam | grep -a '^[<>]' | grep -a -v debug_info | wc -l`
        if [ $lines -gt 0 ]; then
            echo "Difference in: $beam"
        fi
    done
fi
