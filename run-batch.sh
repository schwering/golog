SRC="logs/overtake-0000.log"
DST="logs/current"
N=10
M=30
M=$(cat "$SRC" | wc -l)

for i in $(seq $N $M)
do
        echo $i
        head -n $i "$SRC" > "$DST"
        /usr/bin/time dist/build/Main/Main +RTS -k32M -K128M -H1G -T -p -P -sstderr -RTS
        mv Main.prof Main.prof-$i
done

