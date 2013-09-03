A Golog Interpreter in Haskell
==============================

This is a Golog interpreter written in the purely functional programming
language [Haskell][Haskell].
[Golog][Golog] is an action language based on the [situation calculus][SitCalc]
which is a formalism for reasoning about actions and change.

Besides [toy examples](../golog-examples/) it is currently used for [plan
recognition](../plan-recog/) in traffic domains and [control of a racing
car](../torcs-agent/) in the video game TORCS.

It combines a number of features:

* Transition semantics: programs can be executed step by step.
* Standard imperative constructs: sequence, loop, if-then-else.
* Nondeterministic constructs: branching, concurrency by interleaving (which
  can be locally limited by marking parts as atomic), plus a few macros such as
  pick-best-from-a-list.
* Decision theory: nondeterminism may be resolved by looking for the highest
  reward.
* Real world effects: using Haskell's IO system for side-effects, programs can
  be executed on-line with actions having real world effects. Also, a program
  can be executed off-line first (e.g., to search for the best execution) and
  the result can be synchronized with the real world.
* Progression and regression: the interpreter is indifferent to whether
  progression or regression is used as reasoning mechanism.
  This is an implementation detail hidden to the interpreter.
  This allows for very fast progressive BATs!

The interpreter is implemented in [Golog.Interpreter](src/Golog/Interpreter.hs).

The whole system is written in [Haskell][Haskell].
The interpreter exploits Haskell's laziness to build up a (potentially
infinite) tree of situations and operates on this tree.
The implementation is more compact, powerful, and efficient than previous
prototypes in [ECLiPSe-CLP][ECLiPSe] and [Mercury][Mercury] (see
[here][prGolog-old] for the code), two logical programming languages.


Contact: [Christoph Schwering][HP] (schwering at kbsg.rwth-aachen.de).


[Golog]: http://www.cs.toronto.edu/cogrobo/main/systems/index.html
[SitCalc]: http://en.wikipedia.org/wiki/Situation_calculus
[Haskell]: http://www.haskell.org/
[ECLiPSe]: http://www.eclipseclp.org/
[Mercury]: http://www.mercurylang.org/
[prGolog-old]: https://github.com/schwering/prgolog-old
[HP]: http://www.kbsg.rwth-aachen.de/~schwering/

