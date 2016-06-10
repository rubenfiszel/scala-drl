package drl.policy

import drl.mdp.MDP._
import drl.Rand

class MCTree[S:  Valuable](s: S, val action: Action = null, var total:Float = 0, var simul:Int = 0, var children: List[MCTree[S]]= List()) {

  def bestMove =
    children.maxBy(_.score).action

  def score =
    simul

  def iter() = {
    val lt = select
    val r = lt.last.runSimulation
    lt.foreach(_.add(r))
  }

  def isEmpty =
    simul == 0

  def allStats =
    children.forall(!_.isEmpty)

  def expand() = {
    val aA = s.availableActions
    val gs = aA.map(a => (s.applyTransition(a), a)).toList
    children = gs.map(x => new MCTree[S](x._1._1, x._2))
  }

  def selectScore(psimul:Int) =
    total.toFloat/simul + Math.sqrt(2)*1000*Math.sqrt(Math.log(psimul)/simul)

  def choose(ts: List[MCTree[S]]) =
    ts.maxBy(_.selectScore(simul))

  def add(score: Float) = {
    total += score
    simul += 1
  }

  def select: List[MCTree[S]] =
    if (isEmpty|| !s.canContinue)
      List(this)
    else {
      if (children.isEmpty)
        expand()
      if (allStats)
        this :: choose(children).select
      else
        this :: children(Rand.nextInt(children.length)).select
    }

  def runSimulation = {
    var cs = s
    while (cs.canContinue) {
      cs = RandomQ.applyNext(cs)._1
    }
    cs.value
  }

}
