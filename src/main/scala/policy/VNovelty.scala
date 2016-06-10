package drl.policy

import drl.mdp.MDP._
import drl.backend.Backend._


case class GreedyNoveltyVPolicy[B: NeuralN](model: B, novelty:B, disc:Float, beta:Float, ae:Option[B] = None) extends PolicyV {

  def nextAction[S: Valuable](s: S) = {
    NoveltyPolicy.nextActions(s, model, novelty, disc, beta, ae).maxBy(_._2)._1
  }

}


object NoveltyPolicy {

  def eucDistance(x1: Array[Float], x2:Array[Float]) = {
//    println("DIS: " +x1.mkString(" ") + "\n" + x2.mkString(" "))
    Math.sqrt(x1.zip(x2).map(x => x._1 - x._2).map(x => x*x).sum).toFloat
  }

  def potentials[S: Valuable](l: Seq[((A, (S, Reward, Odd)), Float)], disc:Float) = {
    def potential(e: ((A, (S, Reward, Odd)), Float)) = {
      e._1._2._3*(e._2 + e._1._2._2/disc)
    }
    l.map(potential).sum
  }

  def noveltyDis[S: Valuable, B: NeuralN](ls: List[(S, A, S)], novelty:B, beta:Float, ae: Option[B]) = {
//    val sts = ae.map(_.output())
    val ars =
      if (ae.isEmpty)
        ls.map(x => x._1.inputWithAction(x._2))
      else
        input(ls.map(x => (x._1, x._2)), ae.get)

    val outputAE:Array[Array[Float]] =
      if (ae.isEmpty)
        ls.map(x => x._3.toInput).toArray
      else
        autoEncode(ls.map(x => x._3), ae.get)

    novelty.output(ars.toList, (x: Array[Float]) => x)(0).zip(outputAE).map(x => beta*eucDistance(x._1, x._2))
  }

  def autoEncode[S: Valuable, B: NeuralN](ls: List[S], ae: B) =
//    ls.map(_.toInput.take(10))
    ae.outputHead(ls, 1)

  def input[S: Valuable, B: NeuralN](ls: List[(S, A)], ae: B) =
    autoEncode(ls.map(_._1), ae).zip(ls).map(x => x._1 ++ x._2._1.actionEncoding(x._2._2)).toList

  def nextActions[S: Valuable, B: NeuralN](s: S, model: B, novelty: B, disc: Float, beta:Float, ae: Option[B] = None) = {
    //    val F = implicitly[Statable[S]]
    val aA = s.availableActions
    val ps = aA.map(a => s.potentialStates(a).map(x => (a, x))).flatten
    val states = ps.map(_._2._1)
    //      val noveltys = ps.map(x => x._2._1.inputWithAction(x._1))
    val noveltys:Array[Float] = noveltyDis(ps.map(x => (s, x._1, x._2._1)).toList, novelty, beta, ae)
    val evals = model.outputHead(states.toList, 0).map(_(0)).toArray.zip(noveltys).map(x => x._1 + x._2)

//    println("STATE: " +s)
    println("NOV: " + noveltys.mkString(" "))
    println("EVAL: " + evals.toList)

    ps.zip(evals).groupBy(_._1._1).mapValues(x => potentials[S](x, disc))
  }

}
