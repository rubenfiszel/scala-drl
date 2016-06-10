package drl.policy

import drl.mdp.MDP._

case class UCT(branches: Int = 100) extends PolicyV {

  def nextAction[S: Valuable](s: S) = {
    val mct = new MCTree(s)
    for (i <- 1 to branches)
      mct.iter()
    mct.bestMove
  }

}
