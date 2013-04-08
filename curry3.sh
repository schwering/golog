grep ' 480 ' Main.prof-? | grep curry3 | sed -e 's/Main.prof-//g' | sed -e 's/://g' | while IFS=\  read file func mod id entries rest; do echo $file\;$entries; done >curry3.csv && cat curry3.csv
gnuplot --persist << EOF
set xtics 1
set grid
set datafile separator ";"
plot "curry3.csv" u 1:2 w l lt 1 lw 2
EOF

