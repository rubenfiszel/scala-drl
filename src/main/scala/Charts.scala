package drl

import drl.mdp._

import drl.mdp.MDP._
import drl.backend.Backend._

import org.jfree.chart.renderer.xy._
import scalax.chart.api._
import scalax.chart._

import java.io._

object Charts {
  var series: Map[String, XYSeries] = Map()
  var seriesToFile: Map[String, PrintWriter] = Map()
  var seriesToChart: Map[String, String] = Map()
  var avgs: Map[String, List[Float]] = Map()
  var nbValues: Map[String, Int] = Map()
  var charts: Map[String, XYChart] = Map()

  def createChart(serie: String, chart: String, nbValue: Int) = {
    val f =  new File(serie)
    if (f.exists())
       f.delete()
    f.createNewFile()
    seriesToFile += ((serie, new PrintWriter(new FileOutputStream(serie),true)))
    series +=  ((serie, new XYSeries(serie)))
    seriesToChart += ((serie, chart))
    nbValues += ((serie, nbValue))
    avgs += ((serie, List()))
  }

  def addValue(x:Float, y: Float, serie: String) = {

    val nbAvg = nbValues(serie)
    val ser = series(serie)
    avgs += ((serie, (y::avgs(serie)).take(nbAvg)))
    var ma = avgs(serie).sum/nbAvg
    swing.Swing onEDT {
      ser.add(x, ma)
      seriesToFile(serie).write(x.toString + " " + ma.toString() + "\n")
      seriesToFile(serie).flush()
    }
  }
  def show() = {
    val grp = seriesToChart.toList.groupBy(_._2)
    grp.foreach(x =>
      if (!charts.contains(x._1)) {
        val chart = XYLineChart(x._2.map(y => series(y._1)))
        chart.plot.setRenderer(new XYLineAndShapeRenderer(false, true))
        charts += ((x._1, chart))
        chart.show()
      }
   )
  }

  def save() = {
    charts.foreach(x =>
      x._2.saveAsPDF(x._1)
    )
  }


}
