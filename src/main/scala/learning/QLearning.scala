package drl.learning

import drl.mdp.MDP._
import drl.backend.Backend._
import drl.policy._


object QLearning {


  def ql[S: Statable, B: NeuralN](entry:SARS[S], b:Boolean, head: Int, model: B, target: B, targetNPeriod: Option[Int], zeroImpossible:Boolean, gamma:Float, disc:Float):Array[Float] = {

    def getScore(s:S, head:Int = 0,  cg:B = model):Array[Float] = {
      cg.outputS(s).apply(head)
    }


    def getHeadScore(s:S) =
      getScore(s, head, model)

    val (s1, a, r, s2) = entry

    val ow = implicitly[Statable[S]].outputWidth

    var ar = Array.fill(ow)(0f)
    if (zeroImpossible) {
      val qs = DiscRewardQ.filterPossible(s1, getHeadScore(s1))
      qs.foreach(x =>
        ar(x._2) = x._1
      )
    }
    else
      ar = getHeadScore(s1)

    if (b) {

      if (!s1.canContinue) {
          ar = Array.fill(ow)(0f)
      }

      else  {
        val nqs:Float =
          if (s2.canContinue) {
            if (targetNPeriod.isDefined) {
              val maxA = DiscRewardQ.filterPossible(s2, getHeadScore(s2)).maxBy(_._1)._2
              DiscRewardQ.filterPossible(s2, getScore(s2, head, target)).find(_._2 == maxA).get._1
            }
            else {
              DiscRewardQ.filterPossible(s2, getHeadScore(s2)).maxBy(_._1)._1
            }
          }
          else {
            0f
          }

        val ind = s1.F.actionToIndex(a)
        ar(ind) = (r/disc + gamma*nqs)
        if (!s2.canContinue)
          println("NQS: " + nqs)
      }
    }

    if (false && !s2.canContinue) {
      val hs = getHeadScore(s1)
      println(entry + " " + head)
      println(ar.toList)
      println(hs.toList)

      }
    ar
  }


}
