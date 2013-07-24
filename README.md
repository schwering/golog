prGolog
=======

This is a *plan recognition system* targeted at *automotive traffic*.
It is generally capable of handling *continuous domains* where *multiple agents*
interact with each other and where plan recognition needs to carried out
*on-line*.

If you primarily interested in the Golog interpreter, click
[here][GologInterpreter].

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


Interpreter
-----------

The prGolog interpreter may be useful beyond plan recognition tasks.
It combines a number of features:

* Decision theory: nondeterminism is resolved by looking for the highest reward.
* Transition semantics: programs can be executed step by step.
* Concurrency: two or more programs can be interleaved nondeterministically.
* Atomic complex actions: interleaving can be forbidden on parts of the
  programs.
* Pick-best: given an arbitrary search algorithm, find a reward-maximizing
  value.
* Standard Golog features: sequence, if-then-else, nondeterministic branch,
  while, nondeterministic loop, procedures.
* Progression and regression: the interpreter is indifferent to whether
  progression or regression is used as reasoning mechanism.
  This is an implementation detail of BAT.
  This allows for usually much faster progressive BATs :-).

The whole system is written in [Haskell][Haskell].
The interpreter exploits Haskell's laziness to build up the (potentially)
infinite tree of situations and navigates through this tree.
The prototype is much more compact, powerful, and efficient than previous
prototypes in [ECLiPSe-CLP][ECLiPSe] and [Mercury][Mercury] (see
[here][prGolog-old] for the code), two logical programming languages.

The interpreter is implemented in
[Golog.Interpreter](src/Golog/Interpreter.hs).

The domain-specific code is
[Theorems](src/RSTC/Theorems.hs),
[BAT.Base](src/RSTC/BAT/Base.hs),
[BAT.Regression](src/RSTC/BAT/Regression.hs) and
[BAT.Progression](src/RSTC/BAT/Progression.hs) for the traffic BAT.
Our Golog interpreter makes it easy to implement BATs using either regression
or progression, hence the files BAT.Progression and BAT.Regression.
In our experiments BAT.Progression has been significantly faster than
BAT.Regression.

Contact: [Christoph Schwering][Schwering] (schwering at kbsg.rwth-aachen.de).


[GologInterpreter]: #interpreter
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
[prGolog-old]: https://github.com/schwering/prgolog-old

