package drl.policy

import drl.Rand
import drl.mdp.MDP._
import drl.backend.Backend._

case class EpsGreedyQPolicyHead[B: NeuralN, S: Statable](model: B, k: Int, maxEpsilon: Int) extends EpsGreedy[S, Statable](maxEpsilon, QPolicyHead(model, k))

case class QPolicyHead[B: NeuralN](model: B, k:Int) extends PolicyQ {

  def max[S: Statable](l:Array[Float], s:S) = {
    val (r, n) = DiscRewardQ.findMaxInd(DiscRewardQ.filterPossible(s, l))
//    println(r  + " " + n)// + " " + DiscRewardQ.filterPossible(g, l.toIndexedSeq).mkString(", "))
    n
  }


  def nextAction[S: Statable](s: S) = {
    val o = model.outputHeadS(s, k)
    val r = s.F.allActions(
      max(o,s)
    )
//    println(r + " " + s + " " + s.toInput.toList  + " " + o.toList + " " + k)
    r
  }
}

case class QPolicyCombined[B: NeuralN](model: B, nbHead: Int) extends PolicyQ {


  def max[S: Statable](l:Array[Array[Float]], s: S) = {
    val maxWithInd = (0 until nbHead).map(y => DiscRewardQ.findMaxInd(
      DiscRewardQ.filterPossible(s, (0 until l.head.length).map(x => {
        l(y)(x)
      }).toArray)
    )).groupBy(_._2).map(x => (x._1, x._2.map(_._1)))

    var maxInd, nb = -1
    var maxSum = Float.MinValue
    maxWithInd.foreach(x =>
      if (x._2.length > nb || (x._2.length == nb && x._2.sum > maxSum)) {
        maxInd = x._1
        maxSum = x._2.sum
        nb = x._2.length
      }
    )
/*
    println(" maxInd: " + maxInd )
    println("MAXXX:" + maxWithInd)
    println(l.map(_.toList.map(_.toString.take(5)).mkString(" ")).toList.mkString("\n"))
    println()
*/
    maxInd
  }

  def nextAction[S: Statable](s: S) = {
    s.F.allActions(
      max(
        model.outputS(s),
        s)
    )
  }

}

object DiscRewardQ {

  def findMaxInd(l:Array[(Float, Int)]) =
    l.maxBy(_._1)


  def filterPossible[S: Statable](s: S, mvs: Array[Float]) = {
    val indexs = s.availableActions.map(x => s.F.actionToIndex(x))
    mvs.zipWithIndex.filter(x => indexs.contains(x._2))
  }

}
