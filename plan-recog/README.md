Plan Recognition with Golog
===========================

This is a *plan recognition system* targeted at *automotive traffic*.
It is generally capable of handling *continuous domains* where *multiple agents*
interact with each other and where plan recognition needs to carried out
*on-line*.

If you are primarily interested in the Golog interpreter, click
[here](../golog/).

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
For testing purposes, observations can be recorded and then replayed (using
[scripts/replay.c](scripts/replay.c)).


Contact: [Christoph Schwering][HP] (schwering at gmail dot com).


[SitCalc]: http://en.wikipedia.org/wiki/Situation_calculus
[Golog]: http://www.cs.toronto.edu/cogrobo/main/
[CogRob-2012]: http://schwering.github.io/cogrob-2012.pdf
[CogRob-2012-slides]: http://schwering.github.io/cogrob-2012-slides
[Commonsense-2013]: http://schwering.github.io/commonsense-2013.pdf
[Commonsense-2013-slides]: http://schwering.github.io/commonsense-2013-slides
[TORCS]: http://torcs.sourceforge.net/
[TORCS-robots]: https://github.com/schwering/torcs-drivers
[HP]: http://schwering.github.io

