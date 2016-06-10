package drl.policy

sealed trait Opponent
case object PerfectCollab extends Opponent
case object RandomO extends Opponent

import drl.mdp.MDP._

object TreeSearch {

  type Search[S] = (S, Reward, A)

  def searchScore[S: Statable](s: S, odepth: Int, eval: S => Value, opponent: Opponent, opponentTurn: Boolean = false): Search[S] = {

    def rec(s: S, depth: Int): Search[S] = {

      def isOpponentTurn =
        odepth-depth%2 == 0

      def getMaxScore = {

        var maxMove: A = null
        var max = Float.MinValue
        var leaf: S = s.F.zero

        for (m <- s.availableActions) {
          val ng = s.applyTransition(m)._1
          val search = rec(ng, depth-1)

          val game = search._1
          val scr = search._2

          if (scr > max) {
            maxMove = m
            max = scr
            leaf = game
          }
        }
        (leaf, max, maxMove)
      }

      def getExpectiMax = {

        var sum = 0f
        var leaf: S = s.F.zero
        var maxMove: A = null

        var nb = 0f

        for (m <- s.availableActions) {
          val ng = s.applyTransition(m)._1
          val search = rec(ng, depth-1)

          maxMove = m
          sum += search._2
          nb += 1
          leaf = search._1

        }

        (leaf, sum/nb, maxMove)

      }

      if (depth == 0 || !s.canContinue)
        (s, eval(s), null)
      else
        opponent match {
          case RandomO if opponentTurn => {
            getExpectiMax
          }
          case _ =>
            getMaxScore
        }
    }

    rec(s, odepth)

  }


  def maxmax[S: Statable](s: S, depth: Int, eval: S => Value): Search[S] =
    searchScore(s, depth, eval, PerfectCollab)

  def expectimax[S: Statable](s: S, depth: Int, eval: S => Value): Search[S] =
    searchScore(s, depth, eval, RandomO)

}
