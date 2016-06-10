package drl.policy

import drl.mdp.MDP._

case class TreeSearchP(depth: Int=1, expectimax: Boolean = false, heuristic: Boolean = true) extends PolicyV {

  def nextAction[S: Valuable](s: S) = {

    def eval(s: S): Value =
      if (heuristic)
        s.heuristic
      else
        s.value

    if (expectimax)
      TreeSearch.expectimax(s, depth, eval)._3
    else
      TreeSearch.maxmax(s, depth, eval)._3
  }

}
