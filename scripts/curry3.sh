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

if [ -z "$callid" ]
then
        out="$func"
else
        out="$func-$callid"
fi

rm -f "$out.csv"
for i in $(seq 1 100)
do
        f="Main.prof-$i"
        if [ -f "$f" ]
        then
                if [ -z "$callid" ]
                then
                        n=$(grep " $func " "$f" | awk '{SUM += $4} END {print SUM}')
                else
                        n=$(grep " $func " "$f" | grep " $callid " | awk '{SUM += $4} END {print SUM}')
                fi
                echo "$i;$n" >> $out.csv
        fi
done

gnuplot --persist << EOF
set xtics 1
set grid
set datafile separator ";"
set terminal png size 1024,768
set output "$out.png"
plot "$out.csv" u 1:2 w l lt 1 lw 2
set terminal wxt
plot "$out.csv" u 1:2 w l lt 1 lw 2
EOF

