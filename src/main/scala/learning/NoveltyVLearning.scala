package drl.learning

import drl._
import drl.policy._
import drl.mdp.MDP._
import drl.backend.Backend._

class NoveltyVLearning[S: Valuable, B: NeuralN](vconf: VConf, nconf: Either[NConf, B], novconf: NoveltyConf, offrlconf: OfflineRLConf,  earlytermination: Option[EarlyTermination] = None) extends NoveltyExploration[Valuable, S, B](nconf, novconf, offrlconf, earlytermination){


  lazy val outputWidth = 1

  val novelty:B = buildNoveltyModel(implicitly[Statable[S]].featureSize)

  val pol = new EpsGreedy(offrlconf.maxEpsilon, GreedyNoveltyVPolicy[B](model, novelty, offrlconf.disc, novconf.beta))

  val combined = VPolicyHead(model, 0, offrlconf.disc)

  def fitNovelty(l:List[SARS[S]]) = {
    val targets = l.map(_._4.toInput).toArray
    val inputs = l.map(x => x._1.inputWithAction(x._2))
    novelty.buildAndFit(inputs, Array(targets), (x:Array[Float]) => x)
  }

  def target(l: List[SARS[S]]):Array[Array[Array[Float]]] = {
    val r = Array(TDLambda.tdErr(l, 0, true, model, vconf.lambda, offrlconf.gamma, offrlconf.disc).map(Array(_)))
    fitNovelty(l)
//    TestNovelty.maxt(l)
//    TestNovelty.test(novelty, None)
    r
  }


  def getNext(nb:Int) = {
    sample(pol)
  }


}
