prGolog
=======

This is a *plan recognition system* targeted at *automotive traffic*.
It is generally capable of handling *continuous domains* where *multiple agents*
interact with each other and where plan recognition needs to carried out
*on-line*.

Our approach to plan recognition is as follows.
(See this [paper][CogRob-2012] and these [slides][CogRob-2012-slides] for
details.)

* First the domain is modeled in terms of a [situation calculus][SitCalc] basic
  action theory which basically defines the available primitive actions and
  fluents.
  (This [paper][Commonsense-2013] and these [slides][Commonsense-2013-slides]
  present our theory for reasoning about traffic situations.)
* Then one defines a plan library which consists of [Golog][Golog] programs.
  Each of these programs represents a typical behavior pattern.
* To perform a concrete plan recognition task, execution of these programs is
  simulated.
  The effects in this simulation are compared to the observations of the real
  world.
  If the observations and the simulation are consistent, the respective program
  is considered a potential explanation of the agent's action, i.e., a
  recognized plan.

Our current application domain is automotive traffic.
We use [TORCS][TORCS] as driving simulation.
It transmits observations to the plan recognition twice a second.
The modifications to TORCS can be found in [this repository][TORCS-robots].
For testing purposes, observations can be recorded and replayed (using
[src/replay.c](src/replay.c)).

The prGolog interpreter may be useful beyond plan recognition tasks.
Besides traditional constructs like sequence, if-then-else and loops, it
features nondeterministic branch and iteration, concurrency by interleaving,
and pick of an argument.
The nondeterminism of the latter four constructs is resolved using decision
theory, i.e., the interpreter opts for the "best" (wrt some reward function)
alternative.
The system is written in [Haskell][Haskell].
The implementation is much more compact, powerful and efficient than previous
implementations in [ECLiPSe-CLP][ECLiPSe] and [Mercury][Mercury], two logical
languages.

The central source files are [Golog.hs](src/Interpreter/Golog.hs) and
[Tree.hs](src/Interpreter/Tree.hs) for the Golog interpreter.
The domain-specific code is
[Theorems.hs](src/RSTC/Theorems.hs),
[BAT.Base.hs](src/RSTC/BAT.Base.hs),
[BAT.Regression.hs](src/RSTC/BAT/Regression.hs) and
[BAT.Progression.hs](src/RSTC/BAT/Progression.hs) for the traffic BAT.
Our Golog interpreter makes it easy to implement BATs using either regression
or progression, hence the files BAT.Progression and BAT.Regression.
In our experiments BAT.Progression has been significantly faster than
BAT.Regression.

Contact: [Christoph Schwering][Schwering] (schwering at kbsg.rwth-aachen.de).


[SitCalc]: http://en.wikipedia.org/wiki/Situation_calculus
[Golog]: http://www.cs.toronto.edu/cogrobo/main/
[CogRob-2012]: http://www-kbsg.informatik.rwth-aachen.de/~schwering/CogRob-2012/paper.pdf
[CogRob-2012-slides]: http://www-kbsg.informatik.rwth-aachen.de/~schwering/CogRob-2012/slides.html
[Commonsense-2013]: http://www-kbsg.informatik.rwth-aachen.de/~schwering/Commonsense-2013/paper.pdf
[Commonsense-2013-slides]: http://www-kbsg.informatik.rwth-aachen.de/~schwering/Commonsense-2013/slides.html
[Schwering]: http://www.kbsg.rwth-aachen.de/~schwering/
[Haskell]: http://www.haskell.org/
[ECLiPSe]: http://www.eclipseclp.org/
[Mercury]: http://www.mercurylang.org/
[TORCS]: http://torcs.sourceforge.net/
[TORCS-robots]: https://github.com/schwering/torcs-drivers

