package drl.backend

import org.deeplearning4j.nn.graph.ComputationGraph
import org.nd4j.linalg.api.ndarray.INDArray
import org.nd4j.linalg.dataset._
import java.io._
import org.deeplearning4j.util._

trait SeparableCompGraph {

  def output(ind: INDArray): Array[INDArray]

  def fit(targets: MultiDataSet): Unit

  def cloneM(): SeparableCompGraph

  def save(filename: String): Boolean

  def outputHead(ind: INDArray, head:Int) =
    output(ind).apply(head)

}

class SingleCompGraph(cg: ComputationGraph) extends SeparableCompGraph {

  def output(ind: INDArray) =
    cg.output(ind)

  def fit(targets: MultiDataSet): Unit = {
    cg.fit(targets)
  }

  def cloneM() =
    new SingleCompGraph(cg.clone())

  def save(filename: String) = {
    ModelSerializer.writeModel(cg, new File(filename), true)
    true
  }


}

class SeparatedCompGraph(cgs: IndexedSeq[ComputationGraph]) extends SeparableCompGraph {

  override def outputHead(ind: INDArray, k:Int) =
    cgs(k).output(ind)(0)

  def output(ind: INDArray) =
    cgs.map(_.output(ind)(0)).toArray

  def fit(targets: MultiDataSet): Unit = {
    cgs.zipWithIndex.foreach(x => {
      val (cg, ind) = x
//      println(targets.getFeatures(0))
//      println(targets.getLabels(0))
      cg.fit(new DataSet(targets.getFeatures(0), targets.getLabels(ind)))
    })
  }

  def cloneM() =
    new SeparatedCompGraph(cgs.map(_.clone()))

  def save(filename: String) = {
    false
  }


}
