package drl.mdp

import drl._
import drl.Rand
import drl.mdp.MDP._

sealed trait MoveDeep extends Action
case object RightM extends MoveDeep
case object LeftM extends MoveDeep

case class GameDeepDQN(t: Int, turn: Int, N:Int) {

  def canContinue =
    turn < N + 9

  def move(m: MoveDeep) = {
    val ns = m match {
      case RightM => copy(t = (t+1).min(N), turn =  turn + 1)
      case LeftM => copy(t = (t-1).max(0), turn = turn + 1)
    }
    val reward =
      if (ns.t == 0)
        1f/1000
      else if (ns.t == N)
        1f
      else
        0f
    (ns, reward)
  }

}

case class GameDeepStochDQN(t: Int, turn: Int) {

  val N = 6

  def canContinue =
    turn < N + 9

  def move(m: MoveDeep) = {
    val r = Rand.nextFloat
    val ns = m match {
      case RightM if r < 0.5f=> copy(t = (t+1).min(N), turn = turn + 1)
      case _ => copy(t = (t-1).max(0), turn = turn + 1)
    }
    val reward =
      if (ns.t == 0)
        1f/100
      else if (ns.t == N)
        1f
      else
        0f
    (ns, reward)

  }


}

object GameDeepStochDQN {

    implicit object GameDeepStochDQNS extends Valuable[GameDeepStochDQN] {

    type CAction = MoveDeep

    val allActions = IndexedSeq(RightM, LeftM)

    val zero = GameDeepStochDQN(0, 0)

    def realizeTransition(g: GameDeepStochDQN, m: CAction) = {
      g.move(m)
    }

    def potentialStates(g: GameDeepStochDQN, a: A): IndexedSeq[(GameDeepStochDQN, Reward, Odd)] = {
      val (ng, rw) = realizeTransition(g, cAction(a))
      IndexedSeq((ng, rw, 1f))
    }


    def value(g: GameDeepStochDQN) =
      0f //FIx

    def heuristic(g: GameDeepStochDQN) =
      0f //FIX

    def availableActions(g: GameDeepStochDQN) =
      if (g.canContinue)
        allActions
      else
        IndexedSeq()

    def toInput(g: GameDeepStochDQN) =
      Array.fill(g.t+1)(1f) ++ Array.fill(g.N - g.t)(0f)


    def toString(g: GameDeepStochDQN) =
      g.toString

  }

}

object GameDeepDQN {


  implicit object GameDeepDQNS extends Randomizable[GameDeepDQN] {

    type CAction = MoveDeep

    val allActions = IndexedSeq(RightM, LeftM)

    def zero = GameDeepDQN(0, 0, GameDeepConf.gameL)

    def realizeTransition(g: GameDeepDQN, m: CAction) = {
      g.move(m)
    }

    def potentialStates(g: GameDeepDQN, a: A): IndexedSeq[(GameDeepDQN, Reward, Odd)] = {
      val (ng, rw) = realizeTransition(g, cAction(a))
      IndexedSeq((ng, rw, 1f))
    }

    def genRandom() =
      zero.copy(t = Rand.nextInt(GameDeepConf.gameL), turn = Rand.nextInt(GameDeepConf.gameL+9))

    def value(g: GameDeepDQN) =
      0f //FIx

    def heuristic(g: GameDeepDQN) =
      0f //FIX

    def availableActions(g: GameDeepDQN) =
      if (g.canContinue)
        allActions
      else
        IndexedSeq()

    def toInput(g: GameDeepDQN) =
//      Array.fill(g.t+1)(1f) ++ Array.fill(g.N - g.t)(0f) ++ Array.fill(1)(g.turn/GameDeepConf.gameL.toFloat)
      Array.fill(g.t.min(g.N-1))(0f) ++ Array.fill(1)(1f) ++ Array.fill((g.N - g.t-1).max(0))(0f) //++  Array(g.turn/GameDeepConf.gameL.toFloat)
//      Array(g.t/g.N.toFloat)


    def toString(g: GameDeepDQN) =
      g.toString

  }


}
