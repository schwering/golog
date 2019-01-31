Golog in Haskell and a few Aplications
======================================

This is a Golog interpreter written in Haskell and applications of it.
[Golog](http://www.cs.toronto.edu/cogrobo/main/) is an action language based on
the [situation calculus](http://en.wikipedia.org/wiki/Situation_calculus).
There are many dialects of Golog; this is one of them.

* The [Golog interpreter code](golog/).
* A few [toy examples](golog-examples/) are available.
* A [plan recognition system](plan-recog/), particularly for automotive traffic.
  We model the world with a situation calculus theory and define Golog programs
  which represent typical behavior. The plan recognition system then simulates
  execution of these Golog programs and compares the effects of this simulation
  with observations of the real world.
  [Check out these slides for an overview how it works.](http://schwering.github.io/commonsense-2013-slides/)
* An [agent for a racing car](torcs-agent/).
  The car is controlled by Golog programs and Basic Action Theories.
  This work is at a very early stage.

Contact: [Christoph Schwering](https://schwering.github.io)
(schwering at gmail dot com).

