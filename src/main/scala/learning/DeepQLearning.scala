package drl.learning

import drl._
import drl.policy._
import drl.mdp.MDP._
import drl.backend.Backend._


class DeepQLearning[S: Statable, B: NeuralN](qconf: QConf, nconf:Either[NConf, B], deconf: DeepExplorationConf, offrlconf: OfflineRLConf, earlytermination: Option[EarlyTermination] = None) extends DeepExploration[Statable, S, B](nconf, deconf, offrlconf, earlytermination) {

  val combined = QPolicyCombined(model, deconf.nbHead)

  lazy val outputWidth =
    implicitly[Statable[S]].allActions.length

  val expRepQ = new ExpReplay[S](ExpReplayConf(offrlconf.batchSize, qconf.minPoolFactor, qconf.maxPoolFactor))

  val pols = (0 until deconf.nbHead).map(x => EpsGreedyQPolicyHead(model, x, offrlconf.maxEpsilon))

  def targetHead(l: List[SARS[S]], k:Int):Array[Array[Float]] = {
    l.map(x => {
      val b = (Rand.nextBool(0.5f)) || deconf.nbHead == 1
      val r = QLearning.ql(x, b, k, model, target, deconf.targetNPeriod, qconf.zeroImpossible, offrlconf.gamma, offrlconf.disc)
      r
    }).toArray
  }

  def getNext(nb:Int) = {
    var fetched = List[SARS[S]]()

    val k = Rand.nextInt(deconf.nbHead)
    val (episode, score) = sample(pols(k))

    if (qconf.expRep) {
      expRepQ.add(episode)
      fetched = expRepQ.get(nb)
      expRepQ.clean()
    }
    else {
      fetched = episode
    }
    (fetched, score)
  }

}
