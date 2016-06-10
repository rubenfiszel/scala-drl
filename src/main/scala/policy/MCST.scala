package drl.policy

import drl.mdp.MDP._

case class MCST(branches: Int = 100) extends PolicyQ {

  def mtc[S: Statable](s: S, a: A) = {
    var r = 0f
    for (i <- (1 to branches)) {
      var (cs, rw) = s.applyTransition(a)
      r += rw
      while (cs.canContinue) {
        val na = RandomQ.nextAction(cs)
        var (ns, rw) = cs.applyTransition(na)
        cs = ns
        r += rw
      }
    }
    r
  }

  def nextAction[S: Statable](s: S) = {
    val r =  s.availableActions.map(a => (a, mtc(s, a)))
    println(r)
    r.maxBy(_._2)._1
  }

}
