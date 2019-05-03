/*
 * Copyright LIRIS-CNRS (2017)
 * Contributors: Mohamed Maouche  <mohamed.maouchet@liris.cnrs.fr>HeatMapGenerator.scala
 *
 * Accio is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Accio is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with Accio.  If not, see <http://www.gnu.org/licenses/>.
 */

// @TODO NEED TESTING
package fr.cnrs.liris.privamov.ops

import com.google.inject.Inject
import fr.cnrs.liris.accio.core.api._
import fr.cnrs.liris.common.geo._
import fr.cnrs.liris.privamov.core.model.{Event, Trace}
import fr.cnrs.liris.privamov.core.sparkle.{DataFrame, SparkleEnv}
import com.google.common.geometry._
import java.io._
import java.nio.file.Path

import com.github.nscala_time.time.Imports._
import fr.cnrs.liris.common.util.MathUtils
import org.joda.time.{DateTimeConstants, Duration, Instant}

import scala.util.Random


@Op(
  category = "Attack",
  help = "Re-identification attack based on the matrix matching")
class HeatMapGeneratorOp  extends Operator[HeatMapGeneratorIn, HeatMapGeneratorOut] with SparkleOperator {


  override def execute(in: HeatMapGeneratorIn, ctx: OpContext): HeatMapGeneratorOut = {
    // read the data
    val dstrain = read(in.train, env)
    val dstest = read(in.test, env)


    // rectangular point
    val (p1, p2) = initializePoint(in)
    val (rdstrain, ratio) = restrictArea(dstrain, p1, p2)
    //val (rdstest, ratio2) = restrictArea(dstest, p1, p2)

    val dtTest = in.dtTest match {
      case Some(dur) => dur
      case _ => in.dt
    }


    val dimensions = computeMatricesSize(p1, p2, in.cellSize)
   val  outputMatricesTrain = formMultipleMatrices(dstrain,dimensions,in.cellSize,in.dt)
    val  outputMatricesTest = formMultipleMatrices(dstest,dimensions,in.cellSize,dtTest)
    val n = MathUtils.nextPowerOf2(math.max(outputMatricesTrain.head._2.head.nbCol,outputMatricesTrain.head._2.head.nbRow))
    val allcells = (outputMatricesTrain.values.flatten.flatMap(_.indices).toSet ++ outputMatricesTrain.values.flatten.flatMap(_.indices).toSet).toSeq.sortBy(p => MathUtils.hilbert_xy2d(n,p._1,p._2))


    printHeatMapAllUsers(outputMatricesTrain,allcells,n,ctx.workDir,"train")
    printHeatMapAllUsers(outputMatricesTest,allcells,n,ctx.workDir,"test")

    val nbCells = allcells.size


    val nbDaysMapTrain = outputMatricesTrain.map(p => p._1 -> p._2.length )
    val emptyCellRatioTrain = outputMatricesTrain.map(k => k._1 -> k._2.map(p => p.count(_>0).toDouble/nbCells).sum/k._2.length)
    val avgEmptyCellRatioTrain = emptyCellRatioTrain.values.sum.toDouble / emptyCellRatioTrain.keySet.size

    val nbDaysMapTest = outputMatricesTest.map(p => p._1 -> p._2.length )
    val emptyCellRatioTest = outputMatricesTest.map(k => k._1 -> k._2.map(p => p.count(_>0).toDouble/nbCells).sum/k._2.length)
    val avgEmptyCellRatioTest = emptyCellRatioTest.values.sum.toDouble / emptyCellRatioTest.keySet.size
    HeatMapGeneratorOut(nbCells,nbDaysMapTrain,emptyCellRatioTrain,avgEmptyCellRatioTrain,nbDaysMapTest,emptyCellRatioTest,avgEmptyCellRatioTest)
  }




def printHeatMapAllUsers(outputMatrices : Map[String,Array[MatrixLight[Int]]],allcells : Seq[(Int,Int)],n : Int, workDir : Path,nameFile : String = "heatmaps") = {

  val path = workDir.toAbsolutePath.toString+"HeatMap"
  new File(path).mkdir()
  val file = new File(path+"/"+nameFile+".csv")
  val bw = new BufferedWriter(new FileWriter(file))

  var header = "id,day,nbPoints"
  allcells.foreach{case (i,j) => header = header + s",$i"+"_"+s"$j"+"_"+s"${MathUtils.hilbert_xy2d(n,i,j)}"}
  bw.write(header+"\n")
    outputMatrices.foreach{
      p =>
      val user = p._1
      val heatmaps = p._2
      heatmaps.zipWithIndex.foreach{
        case (hm , k) =>
          val hmp = hm.proportional
          var str = s"$user,$k,${hmp.denominator}"
          allcells.foreach{
            case (i,j) =>
               str = str + s",${hmp(i,j)}"
          }
          bw.write(str+"\n")
      }

  }
bw.close()

}



  def timeToClosestPrecedentHour(t : Instant, hour : Int) : Instant = {
  assert(hour >= 0 || hour < 24)
  var delta = t.toDateTime().getHourOfDay - hour
  if (delta < 0)  delta = 24 - delta
  t.minus( Duration.standardHours(delta) + Duration.standardMinutes(t.toDateTime.getMinuteOfHour))

}

  def formSingleMatrices(ds: DataFrame[Trace], dimensions: (Int, Int, Point), cellSize: Distance): Map[String, MatrixLight[Int]] = {

    var outputMap: Map[String, MatrixLight[Int]] = Map[String, MatrixLight[Int]]().empty
    ds.foreach{ t =>
      synchronized(outputMap += (t.user -> new MatrixLight[Int](dimensions._1, dimensions._2)))
    }
    ds.foreach {
      t =>
        val l = t.events.length

        if (l != 0) {
          t.events.last.time
          val start_trace = t.events.head.time
          val user = t.user
          val events = t.events
          events.foreach { e =>
            val p = e.point
            val j = math.floor((p.x - dimensions._3.x) / cellSize.meters).toInt
            val i = math.floor((p.y - dimensions._3.y) / cellSize.meters).toInt
            val mat = outputMap(user)
            synchronized(mat.inc(i, j))
            synchronized(outputMap += (user -> mat))
          }
        }else{
          synchronized(outputMap  -= t.user)
        }
    }

    outputMap
  }


  def formMultipleMatrices(ds: DataFrame[Trace], dimensions: (Int, Int, Point), cellSize: Distance, dt : Duration): Map[String, Array[MatrixLight[Int]]] = {

    var outputMap: Map[String, Array[MatrixLight[Int]]] = Map[String, Array[MatrixLight[Int]]]().empty

    ds.foreach {
      t =>
        val l = t.events.length
        val id = t.id
        if (l != 0) {
          // start to closer 4AM date
          val start_trace = timeToClosestPrecedentHour(t.events.head.time,4)
          val end_trace = t.events.last.time
          val durationTrace = start_trace to end_trace
          println(s"v1 = ${durationTrace.millis.toDouble/dt.getMillis}, v2= ${durationTrace.millis} ${dt.getMillis} ")
          val nbHM =  math.ceil(durationTrace.millis.toDouble/dt.getMillis).toInt
          println(s"user,$id,nbDays,$nbHM")
          val tab = new Array[MatrixLight[Int]](nbHM)
          for (i <- 0 until nbHM) tab(i) = new MatrixLight[Int](dimensions._1, dimensions._2)
          synchronized(outputMap += (id -> tab))
          val events = t.events
          var setOfHs = Set[Int]()
          val mat = outputMap(id)
          events.foreach { e =>
            val p = e.point
            val j = math.floor((p.x - dimensions._3.x) / cellSize.meters).toInt
            val i = math.floor((p.y - dimensions._3.y) / cellSize.meters).toInt
            val h =  ((start_trace to e.time).millis.toDouble/dt.getMillis).toInt
            if(!setOfHs.contains(h)) {
              println(s"user,$id,nbDays,$nbHM,h,$h")
              setOfHs = setOfHs + h
            }
            // println(s" H =  $h")
            //val mat = outputMap(id)
             mat(h).inc(i, j)
          }
          synchronized(outputMap += (id -> mat))
        }
    }
    outputMap
  }


  def formDayHoursMatrices(ds: DataFrame[Trace], dimensions: (Int, Int, Point), cellSize: Distance): Map[String, Array[MatrixLight[Int]]] = {

    var outputMap: Map[String, Array[MatrixLight[Int]]] = Map[String, Array[MatrixLight[Int]]]().empty
    for (key <- ds.keys) {
      val tab = new Array[MatrixLight[Int]](24)
      for (i <- 0 to 23) tab(i) = new MatrixLight[Int](dimensions._1, dimensions._2)
      outputMap += (key -> tab)
    }
    ds.foreach {
      t =>
        val l = t.events.length
        if (l != 0) {
          t.events.last.time
          val start_trace = t.events.head.time
          val user = t.user
          val events = t.events
          events.foreach { e =>
            val p = e.point
            val j = math.floor((p.x - dimensions._3.x) / cellSize.meters).toInt
            val i = math.floor((p.y - dimensions._3.y) / cellSize.meters).toInt
            val h = e.time.toDateTime.getHourOfDay
            val mat = outputMap(user)
            synchronized(mat(h).inc(i, j))
            synchronized(outputMap += (user -> mat))

          }
        }
    }
    outputMap
  }


  def formDaysMatrices(ds: DataFrame[Trace], dimensions: (Int, Int, Point), cellSize: Distance): Map[String, Array[MatrixLight[Int]]] = {

    var outputMap: Map[String, Array[MatrixLight[Int]]] = Map[String, Array[MatrixLight[Int]]]().empty
    for (key <- ds.keys) {
      val tab2 = new Array[MatrixLight[Int]](7)
      for (i <- 0 to 6) tab2(i) = new MatrixLight[Int](dimensions._1, dimensions._2)
      outputMap += (key -> tab2)
    }
    ds.foreach {
      t =>
        val l = t.events.length
        if (l != 0) {
          t.events.last.time
          val start_trace = t.events.head.time
          val user = t.user
          val events = t.events
          events.foreach { e =>
            val p = e.point
            val j = math.floor((p.x - dimensions._3.x) / cellSize.meters).toInt
            val i = math.floor((p.y - dimensions._3.y) / cellSize.meters).toInt
            val day = e.time.toDateTime.getDayOfWeek - 1
            val mat = outputMap(user)
            mat(day).inc(i, j)
            outputMap += (user -> mat)

          }
        }
    }
    outputMap
  }

  def formWeekendsMatrices(ds: DataFrame[Trace], dimensions: (Int, Int, Point), cellSize: Distance): Map[String, Array[MatrixLight[Int]]] = {

    var outputMap: Map[String, Array[MatrixLight[Int]]] = Map[String, Array[MatrixLight[Int]]]().empty
    for (key <- ds.keys) {
      val tab2 = new Array[MatrixLight[Int]](2)
      for (i <- 0 to 1) tab2(i) = new MatrixLight[Int](dimensions._1, dimensions._2)
      outputMap += (key -> tab2)
    }
    ds.foreach {
      t =>
        val l = t.events.length
        if (l != 0) {
          t.events.last.time
          val start_trace = t.events.head.time
          val user = t.user
          val events = t.events
          events.foreach { e =>
            val p = e.point
            val j = math.floor((p.x - dimensions._3.x) / cellSize.meters).toInt
            val i = math.floor((p.y - dimensions._3.y) / cellSize.meters).toInt
            val day = e.time.toDateTime.getDayOfWeek
            val ind = if (day == DateTimeConstants.SUNDAY || day == DateTimeConstants.SATURDAY) 0 else 1
            val mat = outputMap(user)
            synchronized(mat(ind).inc(i, j))
            synchronized(outputMap += (user -> mat))
          }
        }
    }
    outputMap
  }


  def formWeekEndOnlyMatrice(ds: DataFrame[Trace], dimensions: (Int, Int, Point), cellSize: Distance): Map[String, MatrixLight[Int]] = {

    var trainOutputMap: Map[String, MatrixLight[Int]] = Map[String, MatrixLight[Int]]().empty
    var outputMap: Map[String, MatrixLight[Int]] = Map[String, MatrixLight[Int]]().empty
    for (key <- ds.keys) {
      outputMap += (key -> new MatrixLight[Int](dimensions._1, dimensions._2))
    }
    ds.foreach {
      t =>
        val l = t.events.length
        if (l != 0) {
          //println(s"LENGHT = $l")
          t.events.last.time
          val start_trace = t.events.head.time
          val user = t.user
          val events = t.events
          events.foreach { e =>
            val day = e.time.toDateTime.getDayOfWeek
            if (day == DateTimeConstants.SUNDAY && day == DateTimeConstants.SATURDAY) {
              val p = e.point
              val j = math.floor((p.x - dimensions._3.x) / cellSize.meters).toInt
              val i = math.floor((p.y - dimensions._3.y) / cellSize.meters).toInt
              val mat = outputMap(user)
              synchronized(mat.inc(i, j))
              synchronized(outputMap += (user -> mat))
            }
          }

        }
    }
    outputMap
  }


  def formWeekOnlyMatrice(ds: DataFrame[Trace], dimensions: (Int, Int, Point), cellSize: Distance): Map[String, MatrixLight[Int]] = {

    var outputMap: Map[String, MatrixLight[Int]] = Map[String, MatrixLight[Int]]().empty
    for (key <- ds.keys) {
      outputMap += (key -> new MatrixLight[Int](dimensions._1, dimensions._2))
    }
    // println(s"DIMENSIONS = $dimensions")
    ds.foreach {
      t =>
        val l = t.events.length
        if (l != 0) {
          //println(s"LENGHT = $l")
          t.events.last.time
          val start_trace = t.events.head.time
          val user = t.user
          val events = t.events
          events.foreach { e =>
            val day = e.time.toDateTime.getDayOfWeek
            if (day != DateTimeConstants.SUNDAY && day != DateTimeConstants.SATURDAY) {
              val p = e.point
              val j = math.floor((p.x - dimensions._3.x) / cellSize.meters).toInt
              val i = math.floor((p.y - dimensions._3.y) / cellSize.meters).toInt

              val mat = outputMap(user)
              synchronized(mat.inc(i, j))
              synchronized(outputMap += (user -> mat))

            }
          }

        }
    }
    outputMap
  }


  def formWeekDaysOnlyMatrices(ds: DataFrame[Trace], dimensions: (Int, Int, Point), cellSize: Distance): Map[String, Array[MatrixLight[Int]]] = {

    var outputMap: Map[String, Array[MatrixLight[Int]]] = Map[String, Array[MatrixLight[Int]]]().empty
    for (key <- ds.keys) {
      val tab2 = new Array[MatrixLight[Int]](2)
      for (i <- 0 to 1) tab2(i) = new MatrixLight[Int](dimensions._1, dimensions._2)
      outputMap += (key -> tab2)
    }

    ds.foreach {
      t =>
        val l = t.events.length
        if (l != 0) {
          t.events.last.time
          val start_trace = t.events.head.time
          val user = t.user
          val events = t.events
          events.foreach { e =>
            val p = e.point
            val j = math.floor((p.x - dimensions._3.x) / cellSize.meters).toInt
            val i = math.floor((p.y - dimensions._3.y) / cellSize.meters).toInt
            val day = e.time.toDateTime.getDayOfWeek
            val ind = if (day == DateTimeConstants.SUNDAY || day == DateTimeConstants.SATURDAY) 0 else 1
            val mat = outputMap(user)
            synchronized(mat(ind).inc(i, j))
            synchronized(outputMap += (user -> mat))
          }
        }
    }
    outputMap
  }

  def reIdent(trainMats: Map[String, MatrixLight[Int]], testMats: Map[String, MatrixLight[Int]], n: Int): (Double, Map[String, String]) = {
    // printMatrixCsvfile(testMats)


    var matches: Map[String, String] = Map[String, String]()
    var nbMatches: Int = 0


    testMats.par.foreach {
      //testMats.foreach {
      case (k: String, mat_k: MatrixLight[Int]) =>
        var order = Map[String, Double]()
        val l = trainMats.keys.size
        trainMats.foreach {
          case (u: String, mat_u: MatrixLight[Int]) =>
            val dist =  DistanceUtils.d(mat_k.proportional, mat_u.proportional, n)
            order += (u -> dist)
        }
        val seq = order.toSeq.sortBy(_._2)

        //  println(s" - element $k  trainsMat length =  $l")

        synchronized(matches += (k -> seq.head._1))
        if (k == seq.head._1) nbMatches += 1
    }
    val rate: Double = nbMatches.toDouble / testMats.keys.size.toDouble
    (rate, matches)
  }


  def computeDconv2Save(mats: Map[String, MatrixLight[Int]]): Map[String, (Array[Double], Array[Double], Double, Double)] = {
    var dconv2Save = Map[String, (Array[Double], Array[Double], Double, Double)]()
    mats.foreach { case (k, mat) =>

      val m = mat.proportional
      var dc = 0.0
      val d_i_ = Array.fill[Double](m.nbRow)(0)
      val d_j_ = Array.fill[Double](m.nbCol)(0)
      var d = 0.0
      val indices = m.data.keys.toSet
      var set_i = Set[Int]()
      var set_j = Set[Int]()
      indices.foreach {
        case (i: Int, j: Int) =>
          set_i += i
          set_j += j
          d_i_(i) = d_i_(i) + m(i, j)
          d_j_(j) = d_j_(j) + m(i, j)
          d += m(i, j)
      }
      for (i <- set_i) {
        d_i_(i) = d_i_(i) / m.nbCol.toDouble
      }
      for (j <- set_j) {
        d_j_(j) = d_j_(j) / m.nbRow.toDouble
      }
      d = d / (m.nbRow * m.nbCol).toDouble
      for (i <- set_i) {
        for (j <- set_j) {
          dc = dc + (m(i, j) - d_i_(i) - d_j_(j) - d) * (m(i, j) - d_i_(i) - d_j_(j) - d)
        }
      }
      dc = dc / (m.nbRow * m.nbCol)
      dconv2Save += k -> (d_i_, d_j_, d, dc)
    }
    dconv2Save
  }

  def reIdentMutiple(trainMats: Map[String, Array[MatrixLight[Int]]], testMats: Map[String, Array[MatrixLight[Int]]], n: Int): (Double, Map[String, String]) = {
    // printMatrixMutipleCsvfile(trainMats)

    var matches: Map[String, String] = Map[String, String]()
    var nbMatches: Int = 0
    // println("start foreach")
    testMats.par.foreach {
      //testMats.foreach {
      case (k: String, mat_k: Array[MatrixLight[Int]]) =>
        var order = Map[String, Double]()
        trainMats.foreach {
          case (u: String, mat_u: Array[MatrixLight[Int]]) =>

            var dist = 0.0
            for (ind <- mat_k.indices) {
              dist = dist + DistanceUtils.d(mat_k(ind).proportional, mat_u(ind).proportional, n)
            }
            order += (u -> dist)
        }
        val seq = order.toSeq.sortBy(_._2)
        synchronized(matches += (k -> seq.head._1))

        if (k == seq.head._1) nbMatches += 1
    }
    val rate: Double = nbMatches.toDouble / testMats.keys.size.toDouble
    (rate, matches)
  }


  def printMatrice[U](mat: Map[String, MatrixLight[U]]): Unit = {
    mat.foreach { m =>
      println(s"user : ${m._1} - Nb_(i,j) = ${m._2.data.keys.size}")
      println("-------------------------")
      if (m._2.nbCol > 1000 || m._2.nbRow > 1000) m._2.printNoneEmptyCells() else m._2.printMatrix()
      println("-------------------------")

    }
  }

  def meanFilter(mat: Array[Array[Double]]): Array[Array[Double]] = {
    val newMat = Array.fill[Array[Double]](mat.length)(Array.fill[Double](mat(0).length)(0))
    for (i <- 1 to mat.length - 2) {
      for (j <- 1 to mat(i).length - 2) {
        for (ki <- -1 to 1) {
          for (kj <- -1 to 1) {
            newMat(i)(j) = newMat(i)(j) + mat(i + ki)(j + kj)
          }
        }
        newMat(i)(j) = newMat(i)(j) / 9
      }
    }
    newMat
  }

  def computeMatricesSize(p1: Point, p2: Point, cellSize: Distance): (Int, Int, Point) = {


    val topCornerleft = Point(math.min(p1.x, p2.x), math.max(p1.y, p2.y))
    //val topCornerRight = Point(math.max(p1.x, p2.x), math.max(p1.y, p2.y))
    val bottomCornerleft = Point(math.min(p1.x, p2.x), math.min(p1.y, p2.y))
    val bottomCornerRight = Point(math.max(p1.x, p2.x), math.min(p1.y, p2.y))
    val width = bottomCornerleft.distance(bottomCornerRight)
    val height = topCornerleft.distance(bottomCornerleft)
    val nbRows = math.ceil(height / cellSize).toInt
    val nbColumn = math.ceil(width / cellSize).toInt

    (nbRows, nbColumn, bottomCornerleft)
  }


  def printMatrixCsvfile[U: Numeric](mat: Map[String, MatrixLight[U]]): Unit = {

    mat.foreach { m =>
      val bw = new BufferedWriter(new FileWriter(new File(s"${m._1}.csv")))
      val matrix = m._2.proportional
      var str = "i\\j"

      for (j <- 0 to (matrix.nbCol - 1)) str += s" , $j"
      str += "\n"
      bw.write(str)
      for (i <- 0 to (matrix.nbRow - 1)) {
        str = s"$i"

        for (j <- 0 to (matrix.nbCol - 1)) {
          str += s", ${matrix(i, j)}"
        }
        str += "\n"
        bw.write(str)
      }
      bw.close()
    }
  }

  def printMatrixMutipleCsvfile[U: Numeric](mat: Map[String, Array[MatrixLight[U]]]): Unit = {

    mat.foreach { m =>
      for (k <- m._2.indices) {
        val bw = new BufferedWriter(new FileWriter(new File(s"${m._1}_$k.csv")))
        val matrix = m._2(k).proportional
        var str = "i\\j"
        println(s"${m._1}_$k.csv")
        for (j <- 0 to (matrix.nbCol - 1)) str += s" , $j"
        str += "\n"
        bw.write(str)
        for (i <- 0 to (matrix.nbRow - 1)) {
          str = s"$i"

          for (j <- 0 to (matrix.nbCol - 1)) {
            str += s", ${matrix(i, j)}"
          }
          str += "\n"
          bw.write(str)
        }
        bw.close()
      }
    }
  }

  def printMatrixCsvText[U: Numeric](mat: Map[String, MatrixLight[U]]): Unit = {


    mat.foreach { m =>
      println(s"user : ${m._1}")
      println("-------------------------")
      val matrix = m._2.normalizePositiveMatrix
      print("i\\j")
      for (j <- 0 to (matrix.nbCol - 1)) print(s" , $j")
      println("")
      //  m._2 foreach { row => row foreach print; println }
      for (i <- 0 to (matrix.nbRow - 1)) {
        print(s"$i")
        for (j <- 0 to (matrix.nbCol - 1)) {
          print(s", ${matrix(i, j)}")
        }
        println("")
      }
      println("-------------------------")

    }
  }


  def initializePoint(in: HeatMapGeneratorIn): (Point, Point) = {
    var p1 = Point(0, 0)
    var p2 = Point(0, 0)
    in.lat2 -> in.lng2 match {
      case (Some(lt), Some(lg)) =>
        p1 = LatLng.degrees(in.lat1, in.lng1).toPoint
        p2 = LatLng.degrees(lt, lg).toPoint

      case _ =>
        p1 = LatLng.degrees(in.lat1, in.lng1).toPoint

        p2 = p1.translate(S1Angle.degrees(in.ang), in.diag)

    }
    p1 -> p2
  }

  def emptyCellsRatio[U: Numeric](map: Map[String, MatrixLight[U]]): Double = {
    val tab: Array[Double] = Array.fill[Double](map.keys.size)(0.0)
    var ind: Int = 0
    map.foreach {
      case (k: String, mat: MatrixLight[U]) =>
        tab(ind) = mat.meanZero()
        ind = ind + 1
    }
    val mean = tab.sum / tab.length.toDouble
    mean
  }


  def emptyCellsRatioMultiple[U: Numeric](map: Map[String, Array[MatrixLight[U]]]): Double = {
    val tab: Array[Double] = Array.fill[Double](map.keys.size)(0.0)
    var ind: Int = 0
    map.foreach {
      case (k: String, mat: Array[MatrixLight[U]]) =>
        mat.foreach {
          m => tab(ind) += m.meanZero()
        }
        tab(ind) = tab(ind) / mat.length
        ind = ind + 1
    }
    val mean = tab.sum / tab.length.toDouble
    mean
  }

  def restrictArea(ds: DataFrame[Trace], p1: Point, p2: Point): (DataFrame[Trace], Double) = {
    // Prepare the restrictive box
    val bounder = BoundingBox(p1, p2)
    // Restrict the tracers to the region.
    val output: DataFrame[Trace] = ds.map { t =>
      val newt = t.filter { e => bounder.contains(e.point) }
      newt
    }
    val nbTot =  ds.map(t =>  t.events.size).toArray.sum
    var nbTaken = output.map(t => t.events.size).toArray.sum
    val ratio = nbTaken.toDouble / nbTot.toDouble
    (output, ratio)
  }

  def datasetStats(ds: DataFrame[Trace]): Map[String, Int] = {


    var stats = Map[String, Int]()
    ds.foreach {
      t =>
        if (t.events.nonEmpty) {
          var tracesList = Vector[Seq[Event]]() // list of all taken days
          val user = t.user
          val events = t.events // all the events
          var dayTrace: Seq[Event] = Seq[Event]() // current days list of events
          var day = events.head.time.toDateTime.dayOfYear() // day of start
          for (i <- events.indices) {
            val e = events(i)
            val newDay = e.time.toDateTime.dayOfYear() // current day
            if (newDay == day) {
              dayTrace = dayTrace :+ e
            } else {
              // save the day trace
              if (dayTrace.nonEmpty) {
                tracesList = tracesList :+ dayTrace
                tracesList = tracesList.sortBy(_.size)
              }
              day = newDay
              dayTrace = Seq[Event](e)
            }
            if (i == events.indices.last) {
              // save the day trace
              if (dayTrace.nonEmpty) {
                tracesList = tracesList :+ dayTrace
                tracesList = tracesList.sortBy(_.size)
              }
            }
          }

          tracesList = tracesList.sortBy(_.head.time.toDateTime.getDayOfYear)
          for (k <- tracesList.indices) {
            val l = tracesList(k)
            stats += s"${user}_$k" -> l.size
          }
        }
    }
    stats
  }



  def extractUser(id : String) : String = {
    id.split("-")(0)
  }
}

case class HeatMapGeneratorIn(
                          @Arg(help = "Input  train dataset")
                          train: Dataset,
                          @Arg(help = "Input test dataset")
                          test: Dataset,
                          @Arg(help = "Diagonal size of the restriction area")
                          diag: Distance = Distance.meters(250000),
                          @Arg(help = "Diagona angle (degrees) of the restriction area")
                          ang: Double = 45.0,
                          @Arg(help = "Type of distance metrics between matrices")
                          distanceType: Int = -51,
                          @Arg(help = "Duration of a Heat Map (default 24 hours)")
                          dt: Duration = Duration.standardHours(24),
                          @Arg(help = "Duration of a Heat Map (Default same as train)")
                          dtTest: Option[Duration],
                          @Arg(help = "Type of matrix (temporal aspects)")
                          matrixType: String = "multiple",
                          @Arg(help = "Cell Size in meters")
                          cellSize: Distance,
                          @Arg(help = "Lower point latitude")
                          lat1: Double =  -61.0 ,
                          @Arg(help = "Lower point longitude")
                          lng1: Double = -131.0 ,
                          @Arg(help = "Higher point latitude (override diag & angle)")
                          lat2: Option[Double] ,
                          @Arg(help = "Higher point latitude (override diag & angle)")
                          lng2: Option[Double]
                        )

case class HeatMapGeneratorOut(
                           //@Arg(help = "Matches between users") matches: Map[String, String],
                           @Arg(help = " Number of Considered Cells (ie., at least one heatmap h with h(i,j) != 0") nbCells : Int,
                           @Arg(help = "Train Days per user") daysTrain: Map[String, Int],
                               @Arg(help = "Train Empty cells ratio per user") emptyCellRatioTrain: Map[String, Double],
                              @Arg(help = " Train Average empty cells ratio") avgEmptyCellRatioTrain : Double,
                           @Arg(help = "Test Days per user") daysTest: Map[String, Int],
                           @Arg(help = "Test Empty cells ratio per user") emptyCellRatioTest: Map[String, Double],
                           @Arg(help = "Test Average empty cells ratio") avgEmptyCellRatioTest : Double
                           //@Arg(help = "Ratio points taken by the restriction area") ratioPointsTaken: Double,
                           //@Arg(help = "Mean Ratio of empty cells in matrices") meanEmptyCells: Double,
                          // @Arg(help = "Re-Ident rate") rate: Double
                         )








