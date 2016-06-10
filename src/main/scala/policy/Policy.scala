package drl.policy

import drl.mdp._
import drl.Rand
import drl.mdp.MDP._
import drl.backend.Backend._



trait Policy[G[_] <: Statable[_]]{

  def nextAction[S: G](s: S): A

  def applyNext[S: G: Statable](s: S): (S, R) =
    s.applyTransition(nextAction[S](s))
}

trait PolicyQ extends Policy[Statable]
trait PolicyV  extends Policy[Valuable]


object RandomQ extends PolicyQ {

  def nextAction[S: Statable](s: S) =
    s.randomAction

}

object RandomV extends PolicyV {

  def nextAction[S: Valuable](s: S) =
    s.randomAction

}



case class MixedPolicy[A[_] <: Statable[_]](odd: Float, p1:Policy[A], p2:Policy[A]) extends Policy[A] {

  def nextAction[S: A](s: S) = {
    if (Rand.nextBool(odd))
      p1.nextAction(s)
    else
      p2.nextAction(s)
  }
}


class EpsGreedy[S: Statable, A[S] <: Statable[S]](max:Float, pol:Policy[A]) extends Policy[A] {

  var nb = 0

  def nextAction[S: A](s: S) = {
    nb += 1
    val tresh = 1- (nb.toFloat/max).min(0.99f)
    val rand =  Rand.nextBool(tresh)
    if (rand) {
      val a = s.randomAction
//      println(a + " " + tresh + " " + nb)
      a
    }
   else
     pol.nextAction(s)
  }
}


case class OppPolicy[A[_] <: Statable[_]](p1:Policy[A], p2:Policy[A]) extends Policy[A] {

  var i = -1

  def nextAction[S: A](s: S) = {
    i += 1
    if (i%2 == 0)
      p1.nextAction(s)
    else
      p2.nextAction(s)
  }


}
