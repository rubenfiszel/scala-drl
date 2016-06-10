package drl

import drl.policy._
import drl.mdp._
import drl.learning._
import drl.backend._
import drl.mdp.MDP._
import drl.backend.Backend._
import drl.backend.Backends._

import drl.mdp.Game2048._

object RLApp extends App {

  val seed = 1234567
  val numEx = 40000

  type Game = Game6561
  val fc = Game2048Conf.fullconf(numEx, seed)//GameDeepConf.fullconf(numEx, seed)

//  type Game = GameDeepDQN
//  val fc = GameDeepAEConf.fullconf(numEx, seed)//GameDeepConf.fullconf(numEx, seed)


  Rand.setSeed(seed)





 SelfPlay.trainModelRLDeepQ[Game, SeparableCompGraph](fc.qconf, scala.Left(fc.nconf), fc.deconf, fc.offrlconf)
 //SelfPlay.trainModelRLDeepV[Game, SeparableCompGraph](fc.vconf, scala.Left(fc.nconf), fc.deconf, fc.offrlconf)
 //  SelfPlay.trainNoveltyAEV[Game, SeparableCompGraph](fc.vconf, scala.Left(fc.nconf), fc.novconf, fc.offrlconf)
 // SelfPlay.trainModelRLDeepQ[Game, SeparableCompGraph](fc.qconf, scala.Left(fc.nconf), fc.deconf, fc.offrlconf)
 //  SelfPlay.trainNoveltyV[Game, SeparableCompGraph](fc.vconf, scala.Left(fc.nconf), fc.novconf, fc.offrlconf)

}

/*

  val evalP = TreeSearchP()
  val valP = TreeSearchP(1, false, false)
  val randomP = RandomQ
  val uct = UCT(1000)

  val pl = List(
//    MixedPolicy(1.0f, evalP, randomP),
//    MixedPolicy(0.95f, evalP, randomP),
//    MixedPolicy(0.99f, evalP, randomP)
//    MixedPolicy(0.0f, evalP, randomP),
    TreeSearchP(5, false, true)
//    MixedPolicy(1.0f, valP, randomP)
//    OppPolicy(valP, evalP),
//    OppPolicy(evalP, randomP)
//    uct
  )

  for (p <- pl) {
    ()

  }
 */
