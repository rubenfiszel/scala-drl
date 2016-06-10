package drl.learning

import drl._
import drl.policy._
import drl.mdp.MDP._
import drl.backend.Backend._

trait EarlyTermination
case class RepeatedValue(value:Float, repearted: Int) extends EarlyTermination

case class DeepExplorationConf(
  nbHead: Int = 10,
  targetNPeriod: Option[Int] = None,
  combinedTestFrequency:Int = 5,
  nbAverages:Int = 1
)

abstract class DeepExploration[Q[_] <: Statable[_], S: Q: Statable, B: NeuralN](nconf: Either[NConf, B], deconf: DeepExplorationConf, offrlconf: OfflineRLConf, earlytermination: Option[EarlyTermination] = None) extends OfflineRL[S, B](nconf, offrlconf){

  createCharts()
  var target = model.cloneM()
  val outputWidth: Int
  var scores = List[(Int, Float)]()

  def buildModel(onconf: NConf) =
    buildModel(onconf, outputWidth)

  def buildModel(onconf: NConf, ow: Int) = {
    val backend = implicitly[NeuralN[B]]
    val confnn = ConfNN(
      offrlconf.seed,
      ow,
      onconf.learningRate,
      onconf.l1,
      onconf.l2,
      deconf.nbHead,
      onconf.commonHeight,
      onconf.headHeight,
      onconf.commonWidth,
      onconf.headWidth,
      onconf.momentum,
      onconf.updater,
      onconf.activation
    )
    backend.build[S](confnn)
  }


  def getScores =
    scores

  def combined: Policy[Q]

  def targetHead(l: List[SARS[S]], k:Int): Array[Array[Float]]

  def target(l: List[SARS[S]]):Array[Array[Array[Float]]] =
    (0 until deconf.nbHead).map(k => {
      targetHead(l, k)
    }).toArray

  var repeated = 0
  def next(nb:Int):PreBatch[S] = {

    if (deconf.targetNPeriod.exists(p => nbFetch%p == 0))
      cloneTarget()

    if (nbFetch%deconf.combinedTestFrequency == 0) {
      val avgScore = SelfPlay.average(combined, deconf.nbAverages)._1
      chartCombined(avgScore)
      if (earlytermination.exists(x => x match {
        case RepeatedValue(value, _) if (avgScore >= value) => true
        case _ => false
      }))
        repeated += 1
      else
        repeated = 0

      scores ::= ((nbFetch, avgScore))
    }

    val (episode, score) = getNext(nb)
    chartHead(score)
    val stateList = episode.map(_._1)
    (stateList, target(episode))

  }

  override def hasNext() = {
    super.hasNext() && earlytermination.forall(x => x match {
      case RepeatedValue(_, rep) if (repeated >= rep) =>
        false
      case _ =>
        println(repeated)
        true
    })
  }

  def cloneTarget() = {
    println("CLONED")
    target = model.cloneM
  }


  def createCharts() = {
    Charts.createChart("head-1", "score2", 1)
    Charts.createChart("head-ma10", "score", 100)
    Charts.createChart("combined-1", "score3", 100)
    Charts.show()
  }

  def chartHead(score:Float) {
    Charts.addValue(nbFetch, score, "head-1")
    Charts.addValue(nbFetch, score, "head-ma10")
  }

  def chartCombined(score: Float) {
    Charts.addValue(nbFetch, score, "combined-1")
  }


}
