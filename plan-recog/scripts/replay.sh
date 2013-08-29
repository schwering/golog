f=$1
offset=26
percar=64

if [ ! -f "$f" ]
then
        echo "File $f does not exist. Exiting."
        exit 1
fi

n=$(cat "$f" | wc -L)
m=$(expr \( $n - $offset \) / $percar)

ctrl=$(expr \( $n - $offset \) % $percar)
if [ "$ctrl" != "0" ]
then
        echo "Guessing ($m) is apparently wrong. Exiting."
        exit 1
fi

echo "There should be $m drivers in logfile $f."
./replay -$m "$f"

