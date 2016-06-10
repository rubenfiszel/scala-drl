package drl

import org.slf4j.LoggerFactory

import drl.policy._
import drl.learning._
import drl.mdp.MDP._
import drl.backend.Backend._

object SelfPlay {

  lazy val log = LoggerFactory.getLogger(getClass())


  def trainModelRLDeepQ[S: Statable, B: NeuralN](qconf: QConf, nconf:Either[NConf, B], deconf: DeepExplorationConf, offrlconf: OfflineRLConf, earlyTerm: Option[EarlyTermination] = None) = {

    val qlearning =
        new DeepQLearning[S, B](qconf, nconf, deconf, offrlconf, earlyTerm)

    val model = qlearning.getModel
    model.fit(qlearning)
    qlearning
  }

  def trainModelRLDeepV[S: Valuable, B: NeuralN](vconf: VConf, nconf:Either[NConf, B], deconf: DeepExplorationConf, offrlconf: OfflineRLConf) = {

    val vlearning =
        new DeepVLearning[S, B](vconf, nconf, deconf, offrlconf)

    val model = vlearning.getModel

    model.fit(vlearning)
    model
  }

  def trainNoveltyV[S: Valuable, B: NeuralN](vconf: VConf, nconf:Either[NConf, B], novconf: NoveltyConf, offrlconf: OfflineRLConf) = {

    val vlearning =
      new NoveltyVLearning[S, B](vconf, nconf, novconf, offrlconf)

    val model = vlearning.getModel

    model.fit(vlearning)
    model


  }


    def trainNoveltyAEV[S: Randomizable, B: NeuralN](vconf: VConf, nconf:Either[NConf, B], novconf: NoveltyConf, offrlconf: OfflineRLConf) = {

    val vlearning =
      new NoveltyAEVLearning[S, B](vconf, nconf, novconf, offrlconf)

    val model = vlearning.getModel

    model.fit(vlearning)
    model


  }





  def test[Q[_] <: Statable[_], S: Q: Statable](p:Policy[Q], track: Boolean=false) = {

    val F = implicitly[Statable[S]]
    var s = F.zero
    var r = 0f
    var i = 0
    while (s.canContinue) {
      if (track) {
        log.info(p.getClass.toString + " turn: " + i + " score: " + r)
        println(s)
      }
      val a = p.nextAction(s)
//      println(a)
      val (ns, rw) = s.applyTransition(a)
      s = ns
      r += rw
      i += 1
    }
    println("Score: " + r + " Turn: " + i)
    r
  }


  def average[Q[_] <: Statable[_], S: Q: Statable](p: Policy[Q], nb: Int = 100) = {
    val values = (1 to nb).map(x => test(p))
    (values.sum/values.length.toFloat, values)
  }

}
