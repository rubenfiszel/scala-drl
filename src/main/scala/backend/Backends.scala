package drl.backend


import org.deeplearning4j.nn.graph.ComputationGraph
import org.deeplearning4j.optimize.listeners.ScoreIterationListener
import org.nd4j.linalg.api.ndarray.INDArray
import org.deeplearning4j.util._
import org.nd4j.linalg.factory.Nd4j
import org.nd4j.linalg.dataset._
import java.io._

import drl.mdp.MDP._
import drl.backend.Backend._

object Backends {

  implicit object dl4j extends NeuralN[SeparableCompGraph] {

//    Nd4j.ENFORCE_NUMERICAL_STABILITY = true

    type NN = SeparableCompGraph

    type Batch = MultiDataSet
    type Input = INDArray


    def build[S: Statable](conf: ConfNN, inputSize:Option[Int]):NN  = {
      if (conf.commonHeight > 0) {
        val cgConf = BuildNN.buildCG[S](conf, inputSize)
        val model = new ComputationGraph(cgConf)
        model.init()
        model.setListeners(new ScoreIterationListener(1))
        new SingleCompGraph(model)
      } else {
        println(conf.nbHead + " ....")
        val confs = (1 to conf.nbHead).map(i => conf.copy(seed = conf.seed + i, nbHead = 1))
        val cgConfs = confs.map(c => BuildNN.buildCG[S](c, inputSize))
        val models = cgConfs.map(c => new ComputationGraph(c))
        models.foreach(_.init)
        models.foreach(_.setListeners(new ScoreIterationListener(1)))
        val r = new SeparatedCompGraph(models)
        println(conf.nbHead + " nbHead")
        r
      }
    }

    def buildAE[S: Statable]() =
      BuildNN.buildAE[S]()

    def load(filename: String) = {
      val cg = ModelSerializer.restoreComputationGraph(new File(filename))
      new SingleCompGraph(cg)
    }


    def save(nn: NN, filename: String) =
      nn.save(filename)

    def genInput(ar: Array[Array[Float]]) = {
//      println(ar.length + " " + ar(0).length)
      Nd4j.create(ar)
    }

    def output(nn: NN, inp: Input) = {
      val o = nn.output(inp)
      o.map(h => (0 until h.rows()).map(i => h.getRow(i).dup().data().asFloat()).toArray)

    }


    def buildBatch[A](seqs: List[A], target: Array[Array[Array[Float]]], f: A => Array[Float]): Batch = {
      val in = genInput(seqs.map(f).toArray)
      val out = target.map(ht => Nd4j.create(ht))
      new MultiDataSet(Array(in), out)
    }

    def cloneM(nn: NN) =
      nn.cloneM()

    def fit(nn: NN, b: Batch) = {
//      println(b.getFeatures(0).shape.toList + " " + b.getLabels.map(_.shape.toList).toList)
      nn.fit(b)
//      println("D")
    }


/*    def output[S: Statable](nn: NN, ls: List[S]) = {
      val o = nn.output(genInput(ls))
      o.map(h => (0 until ls.length).map(i => h.getRow(i).dup().data().asFloat()).toArray)
    }
/
    def outputWithAction[S: Statable](nn: NN, ls: List[(S, A)]) = {
      val o = nn.output(genInputWithAction(ls))(0)
      (0 until ls.length).map(i => o.getRow(i).dup().data().asFloat()).toArray
    }


    override def outputHead[S: Statable](nn: NN, ls: List[S], head:Int) = {
      val o = nn.outputHead(genInput(ls), head)
//      println(ls)
      (0 until ls.length).map(i => o.getRow(i).dup().data().asFloat()).toArray
 }
*/

  }

}
