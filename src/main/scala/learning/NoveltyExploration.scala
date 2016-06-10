package drl.learning

import drl._
import drl.policy._
import drl.Charts
import drl.mdp.MDP._
import drl.backend.Backend._

case class NoveltyConf(
  combinedTestFrequency:Int = 5,
  nbAverages:Int = 1,
  beta: Float = 0.01f
)

abstract class NoveltyExploration[Q[_] <: Statable[_], S: Q: Statable, B: NeuralN](nconf: Either[NConf, B], noveltyconf: NoveltyConf, offrlconf: OfflineRLConf, earlytermination: Option[EarlyTermination] = None) extends OfflineRL[S, B](nconf, offrlconf){

  createCharts()

  def outputWidth: Int
  def combined: Policy[Q]

  var scores = List[(Int, Float)]()

  def buildNoveltyModel(inputSize: Int) = {
    val backend = implicitly[NeuralN[B]]
    val F = implicitly[Statable[S]]
    val fS = inputSize + F.outputWidth
    println("Novelty")
    val confnn = ConfNN(
      offrlconf.seed,
      inputSize,
      0.01f,
      None,
      Some(0.05f),
      1,
      0,
      3,
      0,
      128,
      0.9f,
      RMSProp,
      ReLu
    )
    val noveltyModel = backend.build[S](confnn, Some(fS))
    noveltyModel
  }

  def buildModel(onconf: NConf) = {
    println("Model")
    val backend = implicitly[NeuralN[B]]
    val confnn = ConfNN(
      offrlconf.seed,
      outputWidth,
      onconf.learningRate,
      onconf.l1,
      onconf.l2,
      1,
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


  var repeated = 0

  def next(nb:Int):PreBatch[S] = {

    if (nbFetch%noveltyconf.combinedTestFrequency == 0) {
      val avgScore = SelfPlay.average(combined, noveltyconf.nbAverages)._1
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
