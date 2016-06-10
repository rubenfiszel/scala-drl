package drl.learning

import drl._
import drl.mdp.MDP._

case class ExpReplayConf(
  batchSize: Int,
  minPoolFactor: Int,
  maxPoolFactor: Int
)
class ExpReplay[S: Statable](expconf: ExpReplayConf) {

  var memory: List[SARS[S]] = List()
  var memSize = 0

  def reset() = {
    memSize = 0
    memory = List()
  }

  def add(lgame: List[SARS[S]]) = {
    memory :::= lgame
    memSize += lgame.length
  }

  def isEnough =
    memSize >= expconf.batchSize*expconf.minPoolFactor

  def isTooMuch =
    memSize > expconf.batchSize*expconf.maxPoolFactor

  def removeLast = {
    memSize -= 1
    memory = memory.dropRight(1)
  }

  def clean() = {
    while(isTooMuch)
      removeLast
  }

  def get(n: Int) = {

    var fetchSize = 0
    var fetched: List[SARS[S]] = List()

    while (fetchSize < n) {
      val ind = Rand.nextInt(memory.length)
      fetched ::= memory(ind)
      fetchSize += 1
    }
    fetched
  }

}
