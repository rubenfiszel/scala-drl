package drl.policy

import drl.mdp.MDP._
import drl.backend.Backend._

case class EpsGreedyVPolicyHead[B: NeuralN, S: Valuable](model: B, k: Int, maxEpsilon: Int, disc: Float) extends EpsGreedy[S, Valuable](maxEpsilon, VPolicyHead(model, k, disc))

case class VPolicyCombined[B: NeuralN](model: B, disc:Float) extends PolicyV {


  def potentials[S: Valuable](l: Seq[((A, (S, Reward, Odd)), Array[Float])]) = {
    def potential(e: ((A, (S, Reward, Odd)), Array[Float])) = {
      e._1._2._3*(e._2.sum/e._2.length + e._1._2._2/disc)
    }
    l.map(potential).sum
  }

  def nextAction[S: Valuable](s: S) = {
      val aA = s.availableActions
      val ps = aA.map(a => s.potentialStates(a).map(x => (a, x))).flatten
      val states = ps.map(_._2._1)
      val evals = model.output(states.toList).map(_.map(_(0)).toArray)
      val shifted = (0 until evals(0).length).map(y => (0 until evals.length).map(z => evals(z)(y)).toArray).toArray
      ps.zip(shifted).groupBy(_._1._1).mapValues(potentials[S]).maxBy(_._2)._1
  }

}


case class VPolicyHead[B: NeuralN](model: B, k:Int, disc:Float) extends PolicyV {


  def potentials[S: Valuable](l: Seq[((A, (S, Reward, Odd)), Float)]) = {
    def potential(e: ((A, (S, Reward, Odd)), Float)) = {
      e._1._2._3*(e._2 + e._1._2._2/disc)
    }
    l.map(potential).sum
  }


  def nextAction[S: Valuable](s: S) = {

      val aA = s.availableActions
      val ps = aA.map(a => s.potentialStates(a).map(x => (a, x))).flatten
      val states = ps.map(_._2._1)
      val evals = model.outputHead(states.toList, k).map(_(0)).toArray
//      println(s)
//      println(evals.toList)
      val r = ps.zip(evals).groupBy(_._1._1).mapValues(potentials[S]).maxBy(_._2)
      r._1
  }

}
