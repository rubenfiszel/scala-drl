package drl.learning

import drl._
import drl.policy._
import drl.mdp.MDP._
import drl.backend.Backend._


class NoveltyAEVLearning[S: Randomizable, B: NeuralN](vconf: VConf, nconf: Either[NConf, B], novconf: NoveltyConf, offrlconf: OfflineRLConf,  earlytermination: Option[EarlyTermination] = None) extends NoveltyExploration[Valuable, S, B](nconf, novconf, offrlconf, earlytermination){


  lazy val outputWidth = 1

  val load = false


  val novelty:B = buildNoveltyModel(implicitly[Statable[S]].featureSize/2)
  val autoEncode:B =
    if (load)
      implicitly[NeuralN[B]].load("autoencode")
    else
      implicitly[NeuralN[B]].buildAE()

  val pol = GreedyNoveltyVPolicy[B](model, novelty, offrlconf.disc, novconf.beta, Some(autoEncode))

  val combined = VPolicyHead(model, 0, offrlconf.disc)

  if (!load) {
    trainAutoEncoder()
    autoEncode.save("autoencode")
  }

  trainPredict()

  def trainAutoEncoder() =  {

    for (i <- 1 to 20000) {
      println("autoEncoder fit: "+i)
      val inputs = List.fill(1000)(implicitly[Randomizable[S]].genRandom)
      val out2 = autoEncode.outputHead(inputs, 1)
      autoEncode.buildAndFit(inputs, Array(inputs.map(_.toInput).toArray, out2), (x:S) => x.toInput)
    }
  }

  def trainPredict() = {

    def genRandom = {
      val st =  implicitly[Randomizable[S]].genRandom
      val a = st.randomAction
      (st, a)
    }
    for (i <- 1 to 0) {
      println("predict fit: "+i)
      val gen = List.fill(1000)(genRandom)
      val r = gen.map(x => x._1.applyTransition(x._2))
      fitNovelty(gen.zip(r).map(x => (x._1._1, x._1._2, x._2._2, x._2._1)))
    }
  }

  def fitNovelty(l:List[SARS[S]]) = {
    val targets = NoveltyPolicy.autoEncode(l.map(_._4), autoEncode)
    val inputs = NoveltyPolicy.input(l.map(x => (x._1, x._2)), autoEncode)
    val dis2 = NoveltyPolicy.noveltyDis(l.map(x => (x._1, x._2, x._4)), novelty, 1f, Some(autoEncode)).sum
    println("DIS: " + dis2)
    novelty.buildAndFit(inputs, Array(targets), (x:Array[Float]) => x)
  }

  def target(l: List[SARS[S]]):Array[Array[Array[Float]]] = {
    val r = Array(TDLambda.tdErr(l, 0, true, model, vconf.lambda, offrlconf.gamma, offrlconf.disc).map(Array(_)))
    //    trainPredict()
//    TestNovelty.maxt(l)
//    TestNovelty.test(novelty, Some(autoEncode))
    fitNovelty(l)
    r
  }


  def getNext(nb:Int) = {
    sample(pol)
  }


}
