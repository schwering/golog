prGolog
=======

This is a [situation calculus][SitCalc]- and [Golog][Golog]-based system
written in [Haskell][Haskell].
This [paper][CogRob-2012] and [these slides][CogRob-2012-slides]
present the general approach.
Our current application domain is automotive traffic.
This [paper][Commonsense-2013] presents a theory for reasoning about
traffic sitautions.

We have [extended][TORCS-robots] a the racing game [TORCS][TORCS] (via
a so-called robot) to function as driving simulator and observation
source.
The game sends all observations to the plan recognition system.
These observations can also be played back using `src/replay.c`.

Contact: [Christoph Schwering][Schwering] (schwering at kbsg.rwth-aachen.de).


[SitCalc]: http://en.wikipedia.org/wiki/Situation_calculus
[Golog]: http://www.cs.toronto.edu/cogrobo/main/
[CogRob-2012]: http://www-kbsg.informatik.rwth-aachen.de/~schwering/CogRob-2012/PlaRaPeX.pdf
[CogRob-2012-slides]: http://www-kbsg.informatik.rwth-aachen.de/~schwering/CogRob-2012/slides.html
[Commonsense-2013]: http://www-kbsg.informatik.rwth-aachen.de/~schwering/Commonsense-2013/RSTC.pdf
[Schwering]: http://www.kbsg.rwth-aachen.de/~schwering/
[Haskell]: http://www.haskell.org/
[ECLiPSe]: http://www.eclipseclp.org/
[TORCS]: http://torcs.sourceforge.net/
[TORCS-robots]: https://github.com/schwering/torcs-drivers

