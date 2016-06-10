package drl

import drl.backend.Backend._
import drl.learning._

case class NConf(
  learningRate: Float = 0.005f,
  l1: Option[Float] = None,
  l2: Option[Float] = Some(0.005f),
  commonHeight:Int = 0,
  headHeight:Int = 3,
  commonWidth:Int = 128,
  headWidth:Int = 128,
  momentum: Float = 0.95f,
  updater: Updater = RMSProp,
  activation: Activation = ReLu
)


object Game6561Conf {
  val maxTileValue = 7
  val gameL = 100
}

case class FullConf(nconf: NConf, deconf: DeepExplorationConf, offrlconf: OfflineRLConf, qconf: QConf, vconf: VConf, novconf: NoveltyConf = NoveltyConf())

object Game2048Conf {
  val maxTileValue =
    12

  val qconf = QConf(
    expRep = true,
    zeroImpossible = false,
    minPoolFactor = 30,
    maxPoolFactor = 35
  )

  val vconf = VConf(
    lambda = 0.8f
  )

  val nconf = NConf(
    learningRate = 0.005f,
    l1 = None,
    l2 = Some(0.005f),
    commonHeight = 2,
    headHeight = 1,
    commonWidth = 128,
    headWidth = 128,
    momentum = 0.95f,
    updater = RMSProp,
    activation = ReLu
  )

  val deconf = DeepExplorationConf(
    nbHead = 10,
    targetNPeriod = None,
    combinedTestFrequency = 5,
    nbAverages = 1
  )

  def offconf(numEX: Int, seed: Int) =
    OfflineRLConf(numEX, seed, 1000, 300f) //3200

  def fullconf(numEX: Int, seed: Int) =
    FullConf(nconf, deconf, offconf(numEX, seed), qconf, vconf)

}

object GameDeepStochConf {
  val qconf = QConf(true)
  val nconf = NConf()
  val vconf = VConf(
    lambda = 0f
  )


  def offconf(numEX: Int, seed: Int) = OfflineRLConf(numEX, seed, 100, 1f)
  val deconf = DeepExplorationConf(1)
  def fullconf(numEX: Int, seed: Int) = FullConf(nconf, deconf, offconf(numEX, seed), qconf, vconf)
}

object GameDeepConf {

  var gameL =
    20

  val qconf = QConf(
    expRep = true,
    zeroImpossible = false,
    minPoolFactor = 30,
    maxPoolFactor = 35
  )

  val vconf = VConf(
    lambda = 0f
  )


  val nconf = NConf(
    learningRate = 0.005f,
    l1 = None,
    l2 = Some(0.005f),
    commonHeight = 0,
    headHeight = 3,
    commonWidth = 128,
    headWidth = 128,
    momentum = 0.95f,
    updater = RMSProp,
    activation = ReLu
  )

  val deconf = DeepExplorationConf(
    nbHead = 1,
    targetNPeriod = None,
    combinedTestFrequency = 5,
    nbAverages = 1
  )

  def offconf(numEX: Int, seed: Int) =
    OfflineRLConf(numEX, seed, 100, 10f)

  def fullconf(numEX: Int, seed: Int) =
    FullConf(nconf, deconf, offconf(numEX, seed), qconf, vconf)
}



object GameDeepAEConf {

  var gameL =
    20

  val qconf = QConf(
    expRep = true,
    zeroImpossible = false,
    minPoolFactor = 30,
    maxPoolFactor = 35
  )

  val vconf = VConf(
    lambda = 0.8f
  )


  val nconf = NConf(
    learningRate = 0.005f,
    l1 = None,
    l2 = Some(0.005f),
    commonHeight = 0,
    headHeight = 3,
    commonWidth = 128,
    headWidth = 128,
    momentum = 0.95f,
    updater = RMSProp,
    activation = ReLu
  )

  val deconf = DeepExplorationConf(
    nbHead = 1,
    targetNPeriod = None,
    combinedTestFrequency = 5,
    nbAverages = 1
  )

  def offconf(numEX: Int, seed: Int) =
    OfflineRLConf(numEX, seed, 100, 1000f)

  def fullconf(numEX: Int, seed: Int) =
    FullConf(nconf, deconf, offconf(numEX, seed), qconf, vconf)
}
