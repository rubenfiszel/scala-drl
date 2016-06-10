package drl.mdp

import drl._
import drl.mdp.MDP._

case class Game2048(grid: GridIS6561, turn: Int) {

  lazy val toInput = {
    grid.toInput2048
  }

  def canTurn(dir: Direction) = {
    fullMove(Turn(dir))._1.grid != grid
  }

  def move(mv: Move6561) = {

    val ng = mv match {
        case Turn(dir) =>
          val (ng, r) = grid.move(dir)
          (ng, r)
      }

    (Game2048(ng._1, turn+1), ng._2)

  }

  def randomPlace() = {
    val es = grid.emptySpots
    if (!es.isEmpty) {
      val spot = Rand.choose(grid.emptySpots)
      copy(grid = grid.place(spot._1, spot._2, new Piece(1, Red, 2)).get)
    } else
      this
  }

  def fullMove(m: Move6561) = {
    val (ng, r) = move(m)
    (ng.randomPlace(), r)
  }

  def eval =
    grid.eval

  def value =
    grid.value

  lazy val availableMoveNext = {
    if (!grid.emptySpots.isEmpty)
      Game6561.ALL_TURNS filter (x => canTurn(x.dir)) toList
    else
      List()
  }
//    List(availableMoveGen(turn+1).map(x => (x, move(x).get)).maxBy(_._2.grid.eval)._1)

}

object Game2048 {

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
}
