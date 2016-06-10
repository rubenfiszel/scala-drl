package drl.mdp

import drl._
import drl.mdp.MDP._

sealed trait Move6561 extends MDP.Action
case object Start extends Move6561
case class Turn(dir: Direction) extends Move6561
case class Place(x: Int, y: Int, c: Color) extends Move6561


case class Game6561(grid: Grid6561, turn: Int, maxScore: Float) {

  lazy val canContinue =
    turn < Game6561Conf.gameL && !availableMoveNext.isEmpty


  lazy val toInput =
    grid.toInput6561 :+ (1f-turn.toFloat/Game6561Conf.gameL) :+ (turn%5.toFloat)/5

  def canTurn(dir: Direction) =
    move(Turn(dir)).exists(_.grid != grid)



  def move(mv: Move6561) = {

    val ng = mv match {
        case Turn(dir) if Game6561.placeColor(turn).isEmpty =>
          Some(grid.move(dir)._1)
        case Place(x, y, c) if Game6561.placeColor(turn).exists(_ == c) =>
          grid.place(x, y, new Piece(1, c, 3))
      }

      ng.map(g => Game6561(g, turn+1, maxScore.max(grid.value)))

  }


  def eval =
    grid.eval

  def value =
    grid.value

  def randomMove =  {
    val aM = availableMoveNext
    aM(Rand.nextInt(aM.length))
  }

  def applyRandomMove =
    if (canContinue)
      move(randomMove).get
    else
      this

  def applyEvalMove =  {
    val aM = availableMoveNext
    aM.map(m => move(m).get).maxBy(_.grid.eval)
  }



  lazy val availableMoveNext = {
    val aM =
      if (turn == Game6561Conf.gameL)
        List()
      else
        (turn % 5) match {
          case 3 | 4  => Game6561.ALL_TURNS filter (x => canTurn(x.dir)) toList
          case t =>
            val color = Game6561.placeColor(t).get
            grid.emptySpots.map(p => Place(p._1, p._2, color)).toList
        }
    aM
  }
//    List(availableMoveGen(turn+1).map(x => (x, move(x).get)).maxBy(_._2.grid.eval)._1)

}


object Game6561 {

  val ALL_TURNS = IndexedSeq(Turn(Up), Turn(Down), Turn(Right), Turn(Left))

  val colors = List(Blue, Red, Gray)

  val moves = (0 to 51).map( i =>
    if (i < 48)
      Place((i%16)%4, (i%16)/4, colors(i/16))
    else
      ALL_TURNS(i-48)
  )



  def player(turn: Int) =
    (turn+1)%2

  def placeColor(turn: Int): Option[Color] =
    (turn % 5) match {
      case 3 | 4 => None
      case 0 => Some(Blue)
      case 1 => Some(Red)
      case 2 => Some(Gray)
    }


  def newGame(mult:Int) =
    Game6561(Grid6561(mult), 0, 0)


  implicit object Game2048V extends Valuable[Game2048] {

    type CAction = Move6561

    val allActions = Game6561.ALL_TURNS

    val zero = Game2048(Grid6561(2), 0)

    def realizeTransition(g: Game2048, m: CAction) = {
      val ng = g.fullMove(m)
//      (ng, ng.value - g.value)
      ng
    }

    def potentialStates(g: Game2048, a: A): IndexedSeq[(Game2048, Reward, Odd)] = {
      val (ng, rw) = g.move(cAction(a))
      val ngs = ng.grid.emptySpots.map(spot => ng.copy(grid = ng.grid.place(spot._1, spot._2, new Piece(1, Red, 2)).get))
      ngs.map(x => (x, rw, 1f/ngs.length)).toIndexedSeq
    }

    def availableActions(g: Game2048) = {
        g.availableMoveNext
    }

    def value(g: Game2048) =
      g.value

    def heuristic(g: Game2048) =
      g.eval.toFloat

    def toInput(g: Game2048) =
      g.toInput

    def toString(g: Game2048) =
      g.toString

  }

}
