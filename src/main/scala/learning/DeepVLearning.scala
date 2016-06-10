package drl.learning

import drl._
import drl.policy._
import drl.mdp.MDP._
import drl.backend.Backend._

class DeepVLearning[S: Valuable, B: NeuralN](vconf: VConf, nconf: Either[NConf, B], deconf: DeepExplorationConf, offrlconf: OfflineRLConf) extends DeepExploration[Valuable, S, B](nconf, deconf, offrlconf){

  lazy val outputWidth = 1

  val combined = VPolicyCombined(model, offrlconf.disc)

  val pols = (0 until deconf.nbHead).map(x => EpsGreedyVPolicyHead(model, x, offrlconf.maxEpsilon, offrlconf.disc))

  def targetHead(l: List[SARS[S]], k:Int):Array[Array[Float]] = {
    val b = (Rand.nextBool(0.5f)) || deconf.nbHead == 1
    TDLambda.tdErr(l, k, b, model, vconf.lambda, offrlconf.gamma, offrlconf.disc).map(Array(_))
  }

  def getNext(nb:Int) = {
    val k = Rand.nextInt(deconf.nbHead)
    sample(pols(k))
  }


}
