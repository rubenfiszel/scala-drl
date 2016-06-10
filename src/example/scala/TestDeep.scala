package drl

import drl.policy._
import drl.mdp._
import drl.learning._
import drl.mdp.MDP._
import drl.backend.Backend._


object TestDeep {

  def chart100L[S: Statable, B: NeuralN](qconf: QConf, nconf:NConf, deconf: DeepExplorationConf, offrlconf: OfflineRLConf) = {
    Charts.createChart("median", "median", 1)
    Charts.show()
    var scores =  List[(Int, Int)]()
    for (i <- (5 to 100)) {
      GameDeepConf.gameL = i
      val median = medianOver3Seeds(qconf, nconf, deconf, offrlconf)
      scores ::= ((i, median))
      Charts.addValue(i, median, "median")
    }
    println(scores)
    scores
  }

  def optimalIteration(l: List[(Int, Float)]) = {
    var max = 2000
    var current: Int = l.head._1
    var currentL = 0
    for (e <- l) {
      if (currentL == 100 && max == 2000)
        max = current
      if (e._2 >= 9.5f)
        currentL += 1
      else {
        current = e._1
        currentL = 0
      }
    }
    max
  }
  def medianOver3Seeds[S: Statable, B: NeuralN](qconf: QConf, nconf:NConf, deconf: DeepExplorationConf, offrlconf: OfflineRLConf) = {
    val l = (0 to 2)
      .map(x => offrlconf.copy(seed = Rand.nextInt(10000)))
      .map(noffc => SelfPlay.trainModelRLDeepQ(qconf, scala.Left(nconf), deconf, noffc, Some(RepeatedValue(9.5f, 101))).getScores)
      .map(optimalIteration)
      .sorted
    l(1)
  }

}
