package drl.backend

import drl.mdp.MDP._

object Backend {

  type PreBatch[S] = (List[S], Array[Array[Array[Float]]])

  sealed trait Updater
  object RMSProp extends Updater
  object Nesterovs extends Updater

  sealed trait Activation
  object ReLu extends Activation


  case class ConfNN(
    seed: Int,
    outputWidth: Int,
    learningRate: Float = 0.005f,
    l1: Option[Float] = None,
    l2: Option[Float] = Some(0.005f),
    nbHead:Int = 10,
    commonHeight:Int = 3,
    headHeight:Int = 1,
    commonWidth:Int = 64,
    headWidth:Int = 128,
    momentum: Float = 0.9f,
    updater: Updater = Nesterovs,
    activation: Activation = ReLu
  )

  trait NeuralN[NN] {

    type Batch
    type Input

    def build[S: Statable](conf: ConfNN, inputsize:Option[Int]=None): NN

    def buildAE[S: Statable](): NN

    def load(filename: String): NN
    def save(nn: NN, filename: String): Boolean
    def cloneM(nn: NN): NN
    def buildBatch[A](seqs: List[A], target: Array[Array[Array[Float]]], f: A => Array[Float]): Batch

    def buildBatch[S: Statable](seqs: List[S], target: Array[Array[Array[Float]]]): Batch =
      buildBatch(seqs, target, (x:S) => x.toInput)

    def fit(nn: NN, b: Batch): Unit
    def genInput(ar: Array[Array[Float]]): Input

    def output(nn: NN, inp: Input): Array[Array[Array[Float]]]

    def output[A](nn: NN, la: List[A], f: A => Array[Float]):Array[Array[Array[Float]]] =
      output(nn, genInput(la.map(f).toArray))

    def output[S: Statable](nn: NN, ls: List[S]): Array[Array[Array[Float]]] =
      output(nn, ls, (x:S) => x.toInput)

    def outputWithAction[S: Statable](nn: NN, ls: List[(S, A)]): Array[Array[Float]] =
      output(nn, ls, (x: (S,A)) => x._1.inputWithAction(x._2))(0)

    def outputHead[S: Statable](nn: NN, ls: List[S], head:Int) =
      output(nn, ls).apply(head)

  }

  implicit class NeuralNOps[NN: NeuralN](n: NN) {

    val F = implicitly[NeuralN[NN]]

    type FBatch = F.Batch//NeuralN[NN]#Batch

    def buildBatch[A](seqs: List[A], target: Array[Array[Array[Float]]], f: A => Array[Float]): FBatch =
      F.buildBatch(seqs, target, f)

    def save(filename: String): Boolean =
      F.save(n, filename)

    def fitB(b: FBatch):Unit =
      F.fit(n, b)

    def buildAndFit[A](seqs: List[A], target: Array[Array[Array[Float]]], f: A => Array[Float]) = {
      val batch = buildBatch(seqs, target, f)
      fitB(batch)
    }

    def fit(bi: Iterator[FBatch]):Unit =
      bi.foreach(fitB)

    def fit[S: Statable](si: Iterator[PreBatch[S]]):Unit =
      for (s <- si) {
        val b = F.buildBatch(s._1, s._2)
        fitB(b)
      }

    def output[A](la: List[A], f: A => Array[Float]):Array[Array[Array[Float]]] =
      F.output(n, la, f)

    def output[S: Statable](ls: List[S]): Array[Array[Array[Float]]] =
      F.output(n, ls)

    def outputHead[S: Statable](ls: List[S], head:Int): Array[Array[Float]] =
      F.outputHead(n, ls, head)

    def outputHeadS[S: Statable](s: S, head:Int): Array[Float] = {
      outputHead(List(s), head).apply(0)
    }

    def outputWithAction[S: Statable](ls: List[(S, A)]): Array[Array[Float]] =
      F.outputWithAction(n, ls)


    def outputS[S: Statable](s: S): Array[Array[Float]] =
      output(List(s)).map(_(0))

    def cloneM():NN =
      F.cloneM(n)


  }


}
