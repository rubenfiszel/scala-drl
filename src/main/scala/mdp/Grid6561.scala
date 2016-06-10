package drl.mdp

import drl._

sealed trait Color
case object Blue extends Color
case object Red extends Color
case object Gray extends Color


case class Piece(value: Int, color: Color, mult: Int) {

  def combine =
    copy(value = value*mult)

  def log =
    Math.round(Math.log(value.toDouble)/Math.log(mult) + 1).toInt

  override def toString() = {
    val c = color match {
      case Blue => "B"
      case Red => "R"
      case Gray => "G"
    }
    c + log.toString
  }
}

object Grid6561 {

  def apply(mult: Int): GridIS6561 =
    GridIS6561(IndexedSeq.fill(4,4)(None), mult)

  def exponential(max:Int) =
    max-(Math.sqrt(Rand.nextFloat*(max+1)*(max+1))).floor

  def randomPiece6561(nbP:Int, dominant:Color, max:Int, mult: Int) =
    if (Rand.nextInt(16) < nbP) {
      val v = Math.pow(3, exponential(max)).toInt
      val c =
        if (Rand.nextFloat < 0.3)
          dominant
        else
          Rand.choose(Seq(Blue, Red, Gray))

      Some(Piece(v, c, mult))
    }
    else {
      None
    }

  def random(turn:Int, mult:Int): GridIS6561 = {
    val nbP = (Rand.nextInt(17)+Rand.nextInt(17))/2
    val c = Rand.choose(Seq(Blue, Red, Gray))
    val max = (Math.log(turn/5)/Math.log(2)).toInt + 1
    val is =
      GridIS6561(IndexedSeq.fill(4,4)(randomPiece6561(nbP, c, max, mult)), mult)
    if (is.value > turn*4)
      random(turn, mult)
    else
      is
  }

}

/*
grid coordinates expressed as yx:

11 12 13 13
21 22 23 24
31 32 33 34
41 42 43 44

 */

sealed trait Direction
case object Up extends Direction
case object Down extends Direction
case object Right extends Direction
case object Left extends Direction

trait Grid6561  {

  def reward: Int
  def evalOpp: Double
  def eval: Double
  def value: Float
  def toInput2048: Array[Float]
  def toInput6561: Array[Float]
  def allSum: Array[Float]
  def place(x:Int, y: Int, piece: Piece): Option[Grid6561]
  def move(dir: Direction): (Grid6561, Float)
  def emptySpots: Seq[(Int, Int)]
  def get(x:Int, y:Int):Option[Piece]

}



case class GridIS6561(grid: IndexedSeq[IndexedSeq[Option[Piece]]], mult: Int) extends Grid6561 {

  def get(x: Int, y:Int) =
    grid(y)(x)

  def row(y:Int) =
    grid(y)

  def col(x:Int) =
    (0 to 3).map(y => grid(y)(x)).toIndexedSeq

  def all =
    (0 to 15).map(i => grid(i/4)(i%4)).filter(_.isDefined).map(_.get)

  def place(x:Int, y:Int, piece: Piece):Option[GridIS6561] =
    if (get(x, y) == None)
      Some(copy(grid = grid.updated(y, grid(y).updated(x, Some(piece)))))
    else
      None

  def move(dir: Direction = Up) = {

    val ar = Array.fill[Option[Piece]](4, 4)(None)

    val ns =
      (dir == Up || dir == Down) //NORTHSOUTH
    val dr =
      (dir == Down || dir == Right) //DOWNRIGHT
    val range =
      if (dr)
        (3 to 0 by -1)
      else
        (0 to 3)

    var merge = 0f

    for (i <- 0 to 3) {

      var bef:Option[Piece] = None
      var inc =
        if (dr)
          4
        else
          -1

      for (j <- range) {

        val c =
          if (ns)
            grid(j)(i)
          else
            grid(i)(j)


        c match {
          case Some(x) if (bef.exists(_ == x)) =>

            val comb = Some(x.combine)

            merge += comb.get.value

            if (ns)
              ar(inc)(i) = comb
            else
              ar(i)(inc) = comb

            bef = None


          case Some(x) if (bef.exists(_.value == x.value)) =>
            if (ns)
              ar(inc)(i) = None
            else
              ar(i)(inc) = None

            if (dr)
              inc += 1
            else
              inc -= 1

            bef = None


          case sx@Some(x) =>
            if (dr)
              inc -= 1
            else
              inc += 1

            if (ns)
              ar(inc)(i) = sx
            else
              ar(i)(inc) = sx

            bef = sx

          case None => ()
        }
      }
    }
    (copy(grid = ar.map(_.toIndexedSeq).toIndexedSeq), merge)
  }


  def emptySpots =
    for {
      i <- 0 to 3
      j <- 0 to 3 if grid(j)(i) == None
    } yield (i, j)

  def value =
    grid.map(_.map(_.map(_.value).getOrElse(0)).sum).sum

  def edge =
      List(0, 3).map(row(_).map(_.map(_.value).getOrElse(0)).sum).sum +
  List(0, 3).map(col(_).map(_.map(_.value).getOrElse(0)).sum).sum


  def empty_squares = {
    grid.map(_.map(_.map(_.value.toFloat).getOrElse(0.5f)).sum).sum
  }

  def bestSum = {
    allSum.max
  }

  lazy val groupBycolor =
    (Piece(0, Red, mult)::Piece(0, Blue, mult)::Piece(0, Gray, mult)::all.toList).groupBy(_.color).map(x => (x._1, x._2.map(_.value).sum))

  def allSum = {
    val a = groupBycolor.map(_._2)
    a.map(_.toFloat).toArray
//    a.map(x => if(x==a.max) x else 0f)
//    (l(0), l(1), l(2))
  }

  def countMerge(xs:List[Piece], c: Color):Int = {
    lazy val (a, b) = (xs.head, xs.tail.head)
    if (xs.length < 2) 0
    else if (a == b && a.color == c) 2 + countMerge(xs.tail.tail, c)
    else countMerge(xs.tail, c)
  }

  def countBadMerge(xs:List[Piece], c: Color):Int = {
    lazy val (a, b) = (xs.head, xs.tail.head)
    if (xs.length < 2) 0
    else if (a.value == b.value && a.color == c && a.color == b.color ) 2 + countBadMerge(xs.tail.tail, c)
    else countBadMerge(xs.tail, c)
  }


  def countDestroy(xs:List[Piece], c: Color):Int = {
    lazy val (a, b) = (xs.head, xs.tail.head)
    if (xs.length < 2) 0
    else if (a.value == b.value && (a.color == c || b.color == c) && a.color != b.color ) 2 + countDestroy(xs.tail.tail, c)
    else countDestroy(xs.tail, c)
  }

  def countBadDestroy(xs:List[Piece], c: Color):Int = {
    lazy val (a, b) = (xs.head, xs.tail.head)
    if (xs.length < 2) 0
    else if (a.value == b.value && (a.color != c && b.color != c) && a.color != b.color ) 2 + countBadDestroy(xs.tail.tail, c)
    else countBadDestroy(xs.tail, c)
  }

  val p = Array(0, 1, 8, 27, 64, 125, 217, 343)

  def countMonoton(xs:List[Option[Piece]], c: Color):Int = {
    lazy val (a, b) = (xs.head.getOrElse(Piece(0, c, mult)), xs.tail.head)
    if (xs.length < 2) 0
    else if (b.isDefined && b.get.color == c && a.color == c && b.get.value > a.value) {
      p(b.get.log) - p(a.log) + countMonoton(xs.tail, c)
    }
    else countMonoton(xs.tail, c)
  }

  def merges(c: Color) = {
    val l = ((0 to 3).map(row).toList ::: (0 to 3).map(col).toList).map(_.foldLeft(List[Piece]())((acc, pos) => pos.map(_::acc).getOrElse(acc))).map(x => ((countMerge(x, c), countBadMerge(x, c)), (countDestroy(x, c), countBadDestroy(x, c)))).unzip

    val m = ((0 to 3).map(row).toList ::: (0 to 3).map(col).toList).map(_.toList).map(x => countMonoton(x, c).min(countMonoton(x.reverse, c)))


    val l1 = l._1.unzip
    val l2 = l._2.unzip
    (l1._1.sum, l1._2.sum, l2._1.sum, l2._2.sum, m.sum)
  }
  def empties = {
    emptySpots.size
  }

  def reward =
    empties + bestSum.toInt

  def heuristic = {


    val EVAL_EMPTY_WEIGHT        =   1.0;
    val EVAL_MERGE_WEIGHT        =   0.5;
    val EVAL_SUM_WEIGHT          =   2.0;
    val EVAL_DESTROYS_WEIGHT     =   0.75;
    val EVAL_MONOTONOCITY_WEIGHT =   0.001;
    val EVAL_BAD_MERGE_WEIGHT    =   0.5;
    val EVAL_BAD_SUM_WEIGHT      =   0.1;
    val EVAL_BAD_DESTROYS_WEIGHT =   0.2;


    val bestSum = allSum.max
    val bestColor = groupBycolor.filter(_._2 == bestSum).head._1
    val badSum = allSum.sum - bestSum
    val merge = merges(bestColor)

    EVAL_EMPTY_WEIGHT * empties +
    EVAL_MERGE_WEIGHT * merge._1 +
    EVAL_SUM_WEIGHT * bestSum +
    EVAL_BAD_DESTROYS_WEIGHT * merge._4 -
    EVAL_DESTROYS_WEIGHT * merge._3 -
    EVAL_BAD_SUM_WEIGHT * badSum -
    EVAL_BAD_MERGE_WEIGHT * merge._2
//    EVAL_MONOTONOCITY_WEIGHT * merge._5

  }

  def parameters = {
    val bestSum = allSum.max
    val bestColor = groupBycolor.filter(_._2 == bestSum).head._1
    val badSum = allSum.sum - bestSum
    val merge = merges(bestColor)

    Array(empties, bestSum, badSum, merge._1, merge._2, merge._3, merge._4, merge._5).map(_.toFloat)
  }

  def eval =
    heuristic

  def evalOpp =
    bestSum


  def cellToInputColor(opt: Option[Piece], color:Color):Float =
    opt.map(x => {
      if (color == x.color)
        x.value.toFloat
      else
        0f
    }).getOrElse(0f)

  def cellToInput(opt: Option[Piece]):List[Float] =
    opt.map(x => {
      val value = x.value.toFloat
      x.color match {
        case Blue => List(value, 0f, 0f)
        case Red => List(0f, value, 0f)
        case Gray => List(0f, 0f, value)
      }
    }).getOrElse(List(0f, 0f, 0f))


  def pow(i:Int, j:Int) = BigInt(i).pow(j).intValue

  lazy val toInput6561: Array[Float] = {
    val clrs = for {
      c <- List(Blue, Red, Gray)
      i <- 0 to 3
      j <- 0 to 3
    } yield {
      if (grid(i)(j).exists(_.color == c))
        1f
      else
        0f
    }
    val inp = for {
      c <- List(Blue, Red, Gray)
      v <- 0 to Game6561Conf.maxTileValue
      i <- 0 to 3
      j <- 0 to 3
    } yield {
      if (grid(i)(j).equals(Some(Piece(pow(3, v), c, mult))))
        1f
      else {
        0f
      }
    }
    (clrs.toList:::inp.toList).toArray
  }


  lazy val toInput2048: Array[Float] = {
    val inp = for {
      c <- List(Red)
      v <- 0 to Game2048Conf.maxTileValue
      i <- 0 to 3
      j <- 0 to 3
    } yield {
      if (grid(i)(j).equals(Some(Piece(pow(2, v), c, mult))))
        1f
      else {
        0f
      }

    }
    (inp.toArray)
  }


  override def toString() = {
    val ESP = 3
    var str = ""
    str += "value: " + value + "\n"
    for (y <- 0 to 3) {
      str += "| "
      for (x <- 0 to 3) {
        val s = get(x, y).map(_.toString).getOrElse("_")
        val l = s.length/2.0
        str += (" "*ESP).drop(l.floor.toInt) + s + (" "*ESP).drop(l.ceil.toInt)
      }
      str += " |\n"
    }
    str
  }
}
