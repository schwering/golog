#!/bin/bash

declare -A callsOfInterest

callsOfInterest=( ["480"]="curry3" ) 

rm -f entries.csv
touch entries.csv

for callid in "${!callsOfInterest[@]}"
do
        echo "${callsOfInterestg["$callid"]}-$callid;"
done

for i in $(seq 0 100)
do
        if [ -f "Main.prof-$i" ]
        then
                declare -A calls
                cat "Main.prof-$i" | while IFS=\  read constcentre module callid entries time1 alloc1 time2 alloc2 ticks bytes
                do
                        if [ "$func" != "" -a "${callsOfInterest["$callid"]}" == "$costcentre" ]
                        then
                                echo "bad $callid"
                                ${calls["$callid"]}=$entries
                        fi
                done
                for callid in "${!callsOfInterest[@]}"
                do
                        echo "${calls["$callid"]};"
                done
        fi
done
#
#gnuplot --persist << EOF
#set xtics 1
#set grid
#set datafile separator ";"
#plot "entries" u 1:2 w l lt 1 lw 2
#EOF
#

