package drl

import drl.policy._
import drl.mdp._
import drl.learning._
import drl.mdp.MDP._
import drl.backend.Backend._
import java.text.DecimalFormat


object TestNovelty {

  val formatter = new DecimalFormat("#.##")

  val L = 20

  def g(t:Int) =
    GameDeepDQN(t.max(0).min(L), t.max(0), L)

  val games = (-1 to L+1).map(g).toList
/*
  def sas(t:Int) =
    (g(t), RightM, g(t+1))

  def lsas(t:Int) =
    (g(t), RightM, g(t+1).copy(t=(t-2).max(0)))


  val games = (1 to L+5).map(turn =>
    (0 until turn).map(t =>
      sas(t)
    ).toList:::List(lsas(turn)))
 */

  def test[B: NeuralN](novelty: B, autoEncode: Option[B]) {
/*
    games.foreach(l => {
      val targets = NoveltyPolicy.autoEncode(l.map(_._3), autoEncode)
      val inputs = NoveltyPolicy.input(l.map(x => (x._1, x._2)), autoEncode)
      val dis2 = NoveltyPolicy.noveltyDis(l.map(x => (x._1, x._2, x._3)), novelty, 1f, Some(autoEncode)).toList
    println("DIS: " + dis2)
    }
    //  novelty.outputHead(games)
    )
 */

    //val aeg = NoveltyPolicy.autoEncode(games, autoEncode)
    val right = (0 until games.length-2).map(i => (games(1+i), RightM, games(i+2))).toList
    val left = (0 until games.length-2).map(i => (games(i+1), LeftM, games(i))).toList
    val nr = NoveltyPolicy.noveltyDis(right, novelty, 1f, autoEncode).map(formatter.format(_))
    val nl = NoveltyPolicy.noveltyDis(left, novelty, 1f, autoEncode).map(formatter.format(_))
    val r = nl.zip(nr).zip(counts)
    println(r.mkString(" | "))

  }

  val counts = Array.fill(L+1)(0)

  Charts.createChart("maxt", "maxt", 1)
  var i = 0
  def maxt[S: Statable](l:List[SARS[S]]) = {

    i += 1
    val gds = l.map(_._4.asInstanceOf[GameDeepDQN].t)
    val m = gds.max

    gds.foreach(x =>
      counts(x) += 1
    )

    println("MMMMM: " + m)
    Charts.addValue(i, m, "maxt")
  }
  Charts.show()

}
