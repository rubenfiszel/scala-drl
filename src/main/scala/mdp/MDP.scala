package drl.mdp

import drl.Rand

object MDP {

  trait Action

  type Value = Float
  type Reward = Float


  type A = Action
  type R = Reward
  type V = Value
  type SARS[S] = (S, A, R, S)

  type Odd = Float

  trait Statable[State] { //extends Monoid[State]

    type CAction <: Action

    def realizeTransition(state: State, action: CAction): (State, Reward)

    def applyTransition(state: State, action: A): (State, Reward) =
      realizeTransition(state, cAction(action))

    def availableActions(state: State): Seq[A]

//    def zeroMove: A

    def zero: State

    def allActions: IndexedSeq[A]

    lazy val outputWidth =
      allActions.length

    def featureSize =
      toInput(zero).length

    lazy val actionToIndex =
      allActions.zipWithIndex toMap

    def actionEncoding(action: A) = {
      val ind = actionToIndex(action)
      val ar = Array.fill(outputWidth)(0f)
      ar(ind) = 1f
      ar
    }


    def toString(state: State): String

    def toInput(state: State): Array[Float]

    def cAction(a: Action) = a.asInstanceOf[CAction]

  }

  trait Valuable[S] extends Statable[S]{

    def value(state: S): Value

    def heuristic(state: S): Float

    def potentialStates(state: S, action: A): IndexedSeq[(S, Reward, Odd)]

//    def potStates(state: S, action: A): IndexedSeq[(S, Reward, Odd)] =
//      potentialStates(S, cAction(action))

  }

  trait Randomizable[S] extends Valuable[S]{

    def genRandom(): S

  }

  implicit class StatableOps[S: Statable](state: S) {

    val F = implicitly[Statable[S]]

    override def toString =
      F.toString(state)

    def toInput =
      F.toInput(state)

    def applyTransition(action: A) =
      F.applyTransition(state, action)

    def availableActions: Seq[A] =
      F.availableActions(state)

    def randomAction: A =
      Rand.choose(availableActions)

    def availableActionsIndexs =
      availableActions.map(F.actionToIndex)

    def canContinue =
      !(F availableActions(state) isEmpty)

    def actionEncoding(a: Action) =
      F.actionEncoding(a)

    def inputWithAction(a: Action) = {
      val inp = toInput
      val aE = actionEncoding(a)
//      println(inp.length + " " + aE.length)
      inp ++ aE
    }

  }


  implicit class ValuableOps[V: Valuable](v: V) {

    val V = implicitly[Valuable[V]]

    def value =
      V.value(v)

    def heuristic =
      V.heuristic(v)

    def potentialStates(a: A): IndexedSeq[(V, Reward, Odd)] =
      V.potentialStates(v, a)

  }



}
