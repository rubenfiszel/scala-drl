package drl

import util.Random

object Rand {

  var r = new Random(1234)

  def setSeed(i: Int) =
    r.setSeed(i)

  def choose[A](s: Seq[A]):A =
    s(r.nextInt(s.length))

  def nextInt(n: Int) =
    r.nextInt(n)

  def nextBool(f: Float) =
    r.nextFloat() < f

  def nextFloat =
    r.nextFloat


}
