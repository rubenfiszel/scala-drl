[![Build Status](https://travis-ci.org/rubenfiszel/scala-drl.svg?branch=master)](https://travis-ci.org/rubenfiszel/scala-drl)

# Scala Deep Reinforcement Learning

Source code for my [semester project](https://github.com/rubenfiszel/scala-drl/raw/master/report3.pdf) at the LAI (Laboratory of Artificial Intelligence) of EPFL

General Markov Decision Process (MDP) deep reinforcement library on top of deeplearning4j.


##Quickstart

### Implement one of the MDP typeclass

The library use the typeclass pattern: You use your own implementation of your MDP and write a typeclass implementation in the scope to make it usable by the library:

From the most constrained to the less contrained:

Randomizable is a subset of Valuable which is a subset of Statable

Thus, the most easy to implement is Statable.

* Statable enables Q-learning
* Valuable enables TD-Lambda
* Randomizable enables to use the Autoencoder for advanced features

I assume that you have your own implementation of a MDP. Let's take for example 2048. Here is a typeclass implementation:

```scala
implicit object Game6561V extends Randomizable[Game6561] {

	type CAction = Move6561

	val allActions = Game6561.moves

	val zero = Game6561(Grid6561(3), 0, 0)

	def realizeTransition(g: Game6561, m: CAction) = {
		val ng = g.move(m).get
		(ng, ng.value - g.value)
	}

	def potentialStates(g: Game6561, a: A): IndexedSeq[(Game6561, Reward, Odd)] = {
		val (ng, rw) = realizeTransition(g, cAction(a))
		IndexedSeq((ng, rw, 1f))
    }

    def availableActions(g: Game6561) =
      g.availableMoveNext

    def value(g: Game6561) =
      g.value

    def heuristic(g: Game6561) =
      g.eval.toFloat

    def toInput(g: Game6561) =
      g.toInput

    def toString(g: Game6561) =
      g.toString

	def genRandom() =
		Game6561(Grid6561.random(Game6561Conf.gameL, 3), Rand.nextInt(Game6561Conf.gameL), 0)
}
```
Valuable only requires:

```scala
	def value(state: S): Value

    def heuristic(state: S): Float

    def potentialStates(state: S, action: A): IndexedSeq[(S, Reward, Odd)]
```
then to apply Q-learning:

```scala
	import drl.Rand
	import drl.backend._
	import drl.mdp.Game2048._

    Rand.setSeed(seed)

	val nconf:NConf = ...
	val deconf: DeepExplorationConf = ...
	val offrlconf: OfflineRLConf = ...
	val qconf: QConf = ...

	SelfPlay.trainModelRLDeepQ[Game2048, SeparableCompGraph](qconf, scala.Left(nconf), deconf, offrlconf)
```

## MDP included

* 2048
* 6561
* Chain MDP

Easy to add any MDP through the typeclass pattern.


## Current features

* DQN (Q-learning)
* TD-Lambda
* Deep exploration through novelty incentivising
* Deep exploration through bootstrapping DQN
* Monte-Carlo Search Tree
* Minimax and expectimax
* Deeplearning4j backend



## MIT License

Copyright (c) 2016 Ruben Fiszel

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
