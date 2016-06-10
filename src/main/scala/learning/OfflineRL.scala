package drl.learning

import drl._
import drl.policy._
import scala.{Right => ERight, Left => ELeft}
import drl.mdp.MDP._
import drl.backend.Backend._

case class OfflineRLConf(
  numEx: Int,
  seed: Int,
  maxEpsilon: Int = 200,
  disc: Float = 1f,
  batchSize: Int = 32,
  gamma: Float = 0.99f
//  val maskRand = 0.5f//0.5f
)

case class VConf(
  lambda: Float = 0.8f
)

case class QConf(
  expRep: Boolean = true,
  zeroImpossible: Boolean = false,
  minPoolFactor: Int = 30,
  maxPoolFactor: Int = 35
)

abstract class OfflineRL[S: Statable, B: NeuralN](nconf:Either[NConf, B], offRLconf: OfflineRLConf) extends RL[S, B]() {

  type Score = Float

  var nbFetch = 0

  val model = nconf match {
    case ERight(mdl) => mdl
    case ELeft(conf) => buildModel(conf)
  }

  def getModel() = model
  def buildModel(conf: NConf): B
  def getNext(nb:Int): (List[SARS[S]], Score)
  def target(l: List[SARS[S]]):Array[Array[Array[Float]]]

  def sample[Q[_] <: Statable[_], S: Q: Statable](pol:Policy[Q]) = {
    val F = implicitly[Statable[S]]
    val episode = OfflineRL.episode(F.zero)(pol)
    val lv = episode.map(_._3).sum
    println("Ep Score: "+ lv)
    (episode, lv)
  }


  def output[S: Statable](states: IndexedSeq[S], scores:IndexedSeq[Array[Array[Float]]]) = {
    (states, scores.map(_.toIndexedSeq))
  }

  def hasNext() =
    offRLconf.numEx > nbFetch

  def next(n: Int): PreBatch[S]

  def next() = {

    nbFetch += 1

    next(offRLconf.batchSize)

  }

}



object OfflineRL {

  def episode[Q[_] <: Statable[_], S: Q: Statable](s: S)(pol:Policy[Q]) = {
    var exps: List[SARS[S]] = List()
    var cs = s.F.zero
    while (cs.canContinue) {
      val action = pol.nextAction(cs)
      val (ns, reward) = cs.applyTransition(action)
      exps ::= ((cs, action, reward, ns))
      cs = ns
    }
    //    games ::= (cg, NoMove, 0f, cg) //to train to 0f the end
    exps.reverse

  }

}
