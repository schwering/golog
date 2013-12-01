# In the RTS options:
# -hc : by cost centre (?)
# -hd : by constructor
# -hy : by type
# -hm : by module

now=$(date +%Y-%m-%d-%H:%M:%S)

ghc -O2 --make prof/Profiling.hs -prof -auto-all -caf-all -fforce-recomp -rtsopts && \
# It seems he doesn't give a damn about the heap/stack memory limits. Why?
/usr/bin/time prof/Profiling +RTS -hc -p -H500M -K100M && \
hp2ps -e8in -c Profiling.hp && \
convert Profiling.ps -background white "Profiling-$now.png" && \
rm -f Profiling.ps Profiling.hp Profiling.aux Profiling.prof && \
eog "./Profiling-$now.png"
