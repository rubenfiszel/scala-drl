package drl.learning

import drl.mdp.MDP._
import drl.backend.Backend._

object TDLambda {

  def tdErr[S: Valuable, B: NeuralN](l:List[SARS[S]], k:Int, b:Boolean, model:B, lambda:Float, gamma:Float, disc:Float):Array[Float] = {

    val last = l.last._4
    val ls: List[S]  =
      l.map(_._1) :+ last

    val rewards = l.map(_._3)
    val preEv =
        model.output(ls).apply(k).map(_(0))
//      if (!Conf.targetNPeriod.isDefined)
//      else
//        target.output(ls).apply(k).map(_(0))

    val evals = preEv.toArray

    if (!last.canContinue)
      evals(evals.length-1) = 0f

    val dt = (0 until ls.length-1).map(i => rewards(i)/disc + gamma*evals(i+1) - evals(i))

    var r = model.output(ls).apply(k).map(_(0)).init.toArray


    val br = r.toIndexedSeq

    if (b) {

      for (i <- (0 until r.length) ) {
        var ld = 1f
        var j = i
        while (j < r.length && ld > 0.005) {
          r(i) += ld*dt(j)
          ld *= lambda
          j += 1
        }
      }

//      println(evals.toList + " " + rewards + " " + dt + "\n \n")

    }

    r
  }


}
