# First run `./run-batch.sh' to create a sequence of Main.prof-$N files where $N
# is the number of observations handled.
# Then run `./curry3 <func-name> [<func-call-id>]' to plot the graph which shows
# the growth of calls to the given function in the sequence of runs.

func="$1"
callid="$2"

if [ -z "$func" ]
then
        echo "Usage: $0 <func-name> [<func-call-id>]"
        exit 1
fi

rm -f "$func.csv"
for f in Main.prof-*
do
        i=$(echo "$f" | sed -e 's/Main.prof-//g')
        if [ -z "$callid" ]
        then
                n=$(grep " $func " "$f" | awk '{SUM += $4} END {print SUM}')
        else
                n=$(grep " $func " "$f" | grep " $callid " | awk '{SUM += $4} END {print SUM}')
        fi
        echo "$i;$n" >> $func.csv
done

gnuplot --persist << EOF
set xtics 1
set grid
set datafile separator ";"
set terminal png size 1024,768
set output "$func.png"
plot "$func.csv" u 1:2 w l lt 1 lw 2
set terminal wxt
plot "$func.csv" u 1:2 w l lt 1 lw 2
EOF

