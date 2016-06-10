package drl.backend


import java.nio.file.{Files, Paths}
import org.deeplearning4j.nn.conf.inputs.InputType
import org.deeplearning4j.optimize.listeners.ScoreIterationListener
import org.deeplearning4j.nn.graph.ComputationGraph
import org.deeplearning4j.nn.conf.ComputationGraphConfiguration
import org.apache.commons.io.FileUtils
import org.deeplearning4j.datasets.iterator.DataSetIterator
import org.deeplearning4j.datasets.iterator.impl.IrisDataSetIterator
import org.deeplearning4j.eval.Evaluation
import org.deeplearning4j.nn.api.{Layer, OptimizationAlgorithm}
import org.deeplearning4j.nn.conf.layers._
import org.deeplearning4j.nn.conf.{MultiLayerConfiguration, NeuralNetConfiguration, Updater}
import org.deeplearning4j.nn.multilayer.MultiLayerNetwork
import org.deeplearning4j.nn.params.DefaultParamInitializer
import org.deeplearning4j.nn.weights.WeightInit
import org.nd4j.linalg.api.ndarray.INDArray
import org.nd4j.linalg.dataset.{DataSet, SplitTestAndTrain}
import org.nd4j.linalg.factory.Nd4j
import org.nd4j.linalg.lossfunctions.LossFunctions

//import org.deeplearning4j.ui._
//import org.deeplearning4j.ui.weights._
import org.deeplearning4j.nn.api.Model;

import drl.mdp.MDP._
import drl.backend.Backend._

object BuildNN {


  def buildOL(out: Int) = {
    new OutputLayer.Builder(LossFunctions.LossFunction.MSE)
      .nOut(out)
      .weightInit(WeightInit.XAVIER)
      .activation("identity")
      .build()
  }

  def buildOLID(out: Int) = {
    new OutputLayer.Builder(LossFunctions.LossFunction.MSE)
//      .nIn(out)
      .nOut(out)
      .weightInit(WeightInit.XAVIER)
      .learningRate(0f)
      .activation("identity")
      .build()
  }


  def buildDL(in:Int, out:Int, learningRate: Float):DenseLayer = {
      new DenseLayer.Builder()
        .nIn(in)
        .nOut(out)
        .weightInit(WeightInit.RELU)
        .learningRate(learningRate)
        .biasLearningRate(learningRate)
        .activation("relu")
        .build()
  }


  def buildAE[S: Statable]() = {

    val fs = implicitly[Statable[S]].featureSize
    val nccb = new NeuralNetConfiguration.Builder()
    val gb = nccb
      .learningRate(0.01)
      .iterations(1) //makebetter
      .regularization(true)
      .l2(0.05)
      .updater(Updater.NESTEROVS)
      .momentum(0.9f)
      .optimizationAlgo(OptimizationAlgorithm.STOCHASTIC_GRADIENT_DESCENT)
      .seed(1234)
      .graphBuilder()
      .setInputTypes(InputType.feedForward(fs))
      .addInputs("input")

    val layers = List(fs/2, fs/4, fs/2)

    gb.addLayer("L0", buildDL(fs, layers(0), 0.01f), "input")

    (1 until layers.length).foreach(x =>
      gb.addLayer("L"+x, buildDL(layers(x-1), layers(x), 0.01f), "L"+(x-1))
    )

    val h = layers.length

    gb.addLayer("L"+h, buildDL(layers(h-1), fs, 0.01f), "L"+(h-1))

    gb.addLayer("out2", buildOLID(layers(2)), "L2")
    val conf = gb.addLayer("out", buildOL(fs), "L"+h)
      .setOutputs("out", "out2")
      .build()

    println("AE")
    val cg = new ComputationGraph(conf)
    cg.init()
    cg.setListeners(new ScoreIterationListener(1))
    new SingleCompGraph(cg)
  }


  def buildCG[S: Statable](conf: ConfNN, inputsize: Option[Int] = None) = {

    println("CG " + conf.nbHead)
    //    val nccb = splitAndAdd(new NeuralNetConfiguration.Builder())
    val lr =
      if (conf.nbHead > 1)
        2*conf.learningRate
      else
        conf.learningRate

    val nccb = new NeuralNetConfiguration.Builder()
    val nconf = nccb
    //        .learningRate(learningRate)
      .learningRate(lr)
      .iterations(1) //makebetter

    conf.l1.foreach(x =>
      nconf
      .regularization(true)
      .l1(x)
    )

    conf.l2.foreach(x =>
      nconf
        .regularization(true)
        .l2(x)
    )

    conf.updater match {
      case Nesterovs => nconf.updater(Updater.NESTEROVS)
      case RMSProp => nconf.updater(Updater.RMSPROP)
    }

//      .dropOut(0.2)
//      .updater(Updater.ADAGRAD)

    val gb = nconf
      .momentum(conf.momentum)
      .optimizationAlgo(OptimizationAlgorithm.STOCHASTIC_GRADIENT_DESCENT)
      .seed(conf.seed)
      .graphBuilder()


    val fs = inputsize.getOrElse(implicitly[Statable[S]].featureSize)

    val os =
      conf.outputWidth
//        implicitly[Statable[S]].allActions.length

    println("FS: " + fs + " OS:" + os)
    gb
      .setInputTypes(InputType.feedForward(fs))

      .addInputs("input")

    if (conf.commonHeight > 0)
      gb.
        addLayer("L1", buildDL(fs, conf.commonWidth, lr/conf.nbHead), "input")

    for (i <- 2 to conf.commonHeight)
      gb.addLayer("L"+i, buildDL(conf.commonWidth, conf.commonWidth, lr/conf.nbHead), "L"+(i-1))

    if (conf.headHeight > 1) {

      for (i <- (1 to conf.nbHead))
        if (conf.commonHeight > 0)
          gb.addLayer("LH"+i+"-1", buildDL(conf.commonWidth, conf.headWidth, lr), "L"+conf.commonHeight)
        else
          gb.addLayer("LH"+i+"-1", buildDL(fs, conf.headWidth, lr), "input")


      for {
        i <- 1 to conf.nbHead
        j <- 2 to conf.headHeight
      }
        gb.addLayer("LH"+i+"-"+j, buildDL(conf.headWidth, conf.headWidth, lr), "LH"+i+"-"+(j-1))


      for (i <- (1 to conf.nbHead))
        gb.addLayer("out"+i, buildOL(os), "LH"+i+"-"+conf.headHeight)

    } else {

      for (i <- (1 to conf.nbHead))
        gb.addLayer("out"+i, buildOL(os), "L"+conf.commonHeight)
    }

    val outs =
      (1 to conf.nbHead).map(i => "out"+i)

    gb
      .setOutputs(outs:_*)
      .build()


  }


}
