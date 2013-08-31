(Old) Golog for Plan Recognition
================================

This is an *outdated* Golog interpreter primarily intended for plan recognition.
[Here's the new interpreter](../golog/).

There's one feature the old interpreter has which new one doesn't: a pick-best
with search.
This allows to plug in some search algorithm and a evaluation function which
rates the situations reached for certain values, and then the pick-best
construct comes up with the best value.
The essential components of this are the 'Sprout' constructor and the 'best'
function in [Golog.Old.Tree](src/Golog/Old/Tree.hs).


Contact: [Christoph Schwering][HP] (schwering at kbsg.rwth-aachen.de).


[HP]: http://www.kbsg.rwth-aachen.de/~schwering/

