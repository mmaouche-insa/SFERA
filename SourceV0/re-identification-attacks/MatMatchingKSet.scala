/*
 * Copyright LIRIS-CNRS (2017)
 * Contributors: Mohamed Maouche  <mohamed.maouchet@liris.cnrs.fr>MatMatchingKSets.scala
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
import fr.cnrs.liris.privamov.core.model.Trace
import fr.cnrs.liris.privamov.core.sparkle.{DataFrame, SparkleEnv}
import com.google.common.geometry._
import java.io._

import org.joda.time.DateTimeConstants

import scala.collection.immutable

//import _
import scala.util.Random


@Op(
  category = "Attack",
  help = "Re-identification attack based on the matrix matching")
class MatMatchingKSetsOp extends Operator[MatMatchingKSetsIn, MatMatchingKSetsOut] with SparkleOperator {


  override def execute(in: MatMatchingKSetsIn, ctx: OpContext): MatMatchingKSetsOut = {
    // read the data
    val dstrain = read(in.train, env)
    val dstest = read(in.test, env)

    // rectangular point
    val (p1, p2) = initializePoint(in)
    val (rdstrain, ratio) = restrictArea(dstrain, p1, p2)
    val (rdstest, ratio2) = restrictArea(dstest, p1, p2)

    val dimensions = computeMatricesSize(p1, p2, in.cellSize)
    val output = reIdentAttackFullData(rdstrain, rdstest, dimensions, in)
    val rate = output._1
    var matches = output._2
    var klevel = output._3
    var distances = output._4
    var probabilities = output._5
    val listOfUsers = (dstrain.keys.map(getUserFromId) ++ dstest.keys.map(getUserFromId)).toSet.map(trimUser)

    listOfUsers.foreach {
      u: String =>
       /* if (!matches.contains(u))
          matches += (u -> "-")
        if (!klevel.contains(u)) {
          klevel += (u -> listOfUsers.size)
        }
        if (!distances.contains(u)) {
          distances += u -> listOfUsers.toSeq.map(_ -> 1.0).toMap
        }
        if (!probabilities.contains(u)) {
          probabilities += u -> listOfUsers.toSeq.map(_ -> 0.0).toMap
        }*/
        distances.map {
          distance =>
            val newDistance = if (distance._2.contains(u)) distance._2 else distance._2 + (u -> 1.0)
            distance._1 -> newDistance
        }
        probabilities.map {
          probability =>
            val newP = if (probability._2.contains(u)) probability._2 else probability._2 + (u -> 0.0)
            probability._1 -> newP
        }
    }
    print("AP-Attack user re-identified : ")
    println(matches.filter { pair => pair._1 == pair._2 }.keys)
    println(s"AP-Attack rate : $rate")
    MatMatchingKSetsOut(matches, klevel, distances,probabilities, rate)
  }

  def reIdentAttack(rdstrain: DataFrame[Trace], rdstest: DataFrame[Trace], dimensions: (Int, Int, Point), in: MatMatchingKSetsIn): (Double, Map[String, String], Double) = {
    val ((rate, matches), meantrain) = in.matrixType match {
      case "full" => {
        val trainOutputMap = formSingleMatrices(rdstrain, dimensions, in.cellSize)
        val testOutputMap = formSingleMatrices(rdstest, dimensions, in.cellSize)
        emptyCellsRatio(trainOutputMap)
        (reIdent(trainOutputMap, testOutputMap, in.distanceType), emptyCellsRatio(trainOutputMap))
      }
      case "days" => {
        val trainOutputMap = formDaysMatrices(rdstrain, dimensions, in.cellSize)
        val testOutputMap = formDaysMatrices(rdstest, dimensions, in.cellSize)

        (reIdentMutiple(trainOutputMap, testOutputMap, in.distanceType), emptyCellsRatioMultiple(trainOutputMap))
      }
      case "hours" => {
        val trainOutputMap = formDayHoursMatrices(rdstrain, dimensions, in.cellSize)
        val testOutputMap = formDayHoursMatrices(rdstest, dimensions, in.cellSize)
        (reIdentMutiple(trainOutputMap, testOutputMap, in.distanceType), emptyCellsRatioMultiple(trainOutputMap))
      }
      case "week_weekend" => {
        val trainOutputMap = formWeekendsMatrices(rdstrain, dimensions, in.cellSize)
        val testOutputMap = formWeekendsMatrices(rdstest, dimensions, in.cellSize)
        (reIdentMutiple(trainOutputMap, testOutputMap, in.distanceType), emptyCellsRatioMultiple(trainOutputMap))
      }
      case "weekOnly" => {
        val trainOutputMap = formWeekOnlyMatrice(rdstrain, dimensions, in.cellSize)
        val testOutputMap = formWeekOnlyMatrice(rdstest, dimensions, in.cellSize)
        (reIdent(trainOutputMap, testOutputMap, in.distanceType), emptyCellsRatio(trainOutputMap))
      }
      case "weekEndOnly" => {
        val trainOutputMap = formWeekEndOnlyMatrice(rdstrain, dimensions, in.cellSize)
        val testOutputMap = formWeekEndOnlyMatrice(rdstest, dimensions, in.cellSize)
        (reIdent(trainOutputMap, testOutputMap, in.distanceType), emptyCellsRatio(trainOutputMap))
      }
    }

    (rate, matches, meantrain)
  }

  def reIdentAttackFullData(rdstrain: DataFrame[Trace], rdstest: DataFrame[Trace], dimensions: (Int, Int, Point), in: MatMatchingKSetsIn) = {
    val trainOutputMap = formSingleMatrices(rdstrain, dimensions, in.cellSize)
    val testOutputMap = formSingleMatricesByTraceId(rdstest, dimensions, in.cellSize)
    reIdentFullData(trainOutputMap, testOutputMap, in.distanceType, (rdstrain.keys.map(getUserFromId(_)).toSet ++ rdstest.keys.map(getUserFromId(_)).toSet).map(trimUser))
  }


  def formSingleMatrices(ds: DataFrame[Trace], dimensions: (Int, Int, Point), cellSize: Distance): Map[String, MatrixLight[Int]] = {

    var outputMap: Map[String, MatrixLight[Int]] = Map[String, MatrixLight[Int]]().empty
    ds.foreach { t =>
      synchronized(outputMap += (trimUser(t.user) -> new MatrixLight[Int](dimensions._1, dimensions._2)))
    }
    ds.foreach {
      t =>
        val l = t.events.length

        if (l != 0) {
          t.events.last.time
          val start_trace = t.events.head.time
          val user = trimUser(t.user)
          val events = t.events
          events.foreach { e =>
            val p = e.point
            val j = math.floor((p.x - dimensions._3.x) / cellSize.meters).toInt
            val i = math.floor((p.y - dimensions._3.y) / cellSize.meters).toInt
            val mat = outputMap(user)
            synchronized(mat.inc(i, j))
            synchronized(outputMap += (user -> mat))
          }
        } else {
          synchronized(outputMap -= t.user)
        }
    }

    outputMap
  }


  def formSingleMatricesByTraceId(ds: DataFrame[Trace], dimensions: (Int, Int, Point), cellSize: Distance): immutable.Map[String, MatrixLight[Int]] = {

    var outputMap: Map[String, MatrixLight[Int]] = Map[String, MatrixLight[Int]]().empty
    ds.foreach { t =>
      synchronized(outputMap += (t.id -> new MatrixLight[Int](dimensions._1, dimensions._2)))
    }
    ds.foreach {
      t =>
        val l = t.events.length
        if (l != 0) {
          t.events.last.time
          val start_trace = t.events.head.time
          val id = t.id
          val events = t.events
          events.foreach { e =>
            val p = e.point
            val j = math.floor((p.x - dimensions._3.x) / cellSize.meters).toInt
            val i = math.floor((p.y - dimensions._3.y) / cellSize.meters).toInt
            val mat = outputMap(id)
            mat.inc(i, j)
            synchronized{outputMap += (id -> mat)}
          }
        }else {
          synchronized(outputMap -= t.id)
        }
    }
    outputMap
  }

  def trimUser(s: String): String = {
    try {
      s.toInt.toString
    } catch {
      case e: Exception => s
    }
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
            val dist = DistanceUtils.d(mat_k.proportional, mat_u.proportional, n)
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

  def reIdentFullData(trainMats: Map[String, MatrixLight[Int]], testMats: Map[String, MatrixLight[Int]], n: Int, setOfUsers: Set[String]) = {
    // printMatrixCsvfile(testMats)


    var matches: Map[String, String] = Map[String, String]()
    var distances: Map[String, Map[String, Double]] = Map[String, Map[String, Double]]()
    var probabilites: Map[String, Map[String, Double]] = Map[String, Map[String, Double]]()

    var klevel: Map[String, Int] = Map[String, Int]()
    var nbMatches: Int = 0

    println(s"list of user size = ${setOfUsers.size}  and equal \n  $setOfUsers")
    testMats.par.foreach {
      //testMats.foreach {
      case (k: String, mat_k: MatrixLight[Int]) =>
        var order = Map[String, Double]()
        trainMats.foreach {
          case (u: String, mat_u: MatrixLight[Int]) =>
            val dist = DistanceUtils.d(mat_k.proportional, mat_u.proportional, n)
            order += (trimUser(u) -> dist)
        }
        setOfUsers.foreach(o => if (!order.contains(o)) order += (o -> 1.0))
        //val seq = order.toSeq.sortBy(_._2)
        // BE SURE THAT THE MEASURE IS BOUND IN [0,1]
        val minDistance = order.values.min
        val maxDistance = order.values.max

        val similarities = order.map(o => o._1 -> (1 - normelize(o._2, minDistance, maxDistance)))
        val sumSimilarites = similarities.values.sum
        var prob = similarities.map(o => o._1 -> o._2 / sumSimilarites)


        //setOfUsers.foreach(o => if (!order.contains(o)) order += (o -> 1.0))
        val seq = order.toSeq.sortWith((left, right) => left._2 < right._2 || (left._2 == right._2 && right._1 == k))
        val seqId = seq.map(_._1)
        var level = seqId.indexOf(trimUser(getUserFromId(k))) + 1
        if (level == 0) level = setOfUsers.size
        //  println(s" - element $k  trainsMat length =  $l")
        synchronized(distances += (k -> seq.toMap))
        synchronized(probabilites += (k -> prob))
        synchronized(klevel += (k -> level))
        synchronized(matches += (k -> seq.head._1))
        if (trimUser(getUserFromId(k)) == seq.head._1) nbMatches += 1
    }
    val rate: Double = nbMatches.toDouble / testMats.keys.size.toDouble
    (rate, matches, klevel, distances, probabilites)
  }

  def getUserFromId(id : String) : String = id.split('-')(0)

  def normelize(value: Double, minV: Double, maxV: Double): Double = (value - minV) / (maxV - minV)

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


  def initializePoint(in: MatMatchingKSetsIn): (Point, Point) = {
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
    val nbTot = ds.map(t => t.events.size).toArray.sum
    var nbTaken = output.map(t => t.events.size).toArray.sum
    val ratio = nbTaken.toDouble / nbTot.toDouble
    (output, ratio)
  }

  def extractUser(id: String): String = {
    id.split("-")(0)
  }
}

case class MatMatchingKSetsIn(
                               @Arg(help = "Input train dataset")
                               train: Dataset,
                               @Arg(help = "Input test dataset")
                               test: Dataset,
                               @Arg(help = "Diagonal size of the restriction area")
                               diag: Distance = Distance.meters(250000),
                               @Arg(help = "Diagona angle (degrees) of the restriction area")
                               ang: Double = 45.0,
                               @Arg(help = "Type of distance metrics between matrices")
                               distanceType: Int = 2,
                               @Arg(help = "Type of matrix (temporal aspects)")
                               matrixType: String = "full",
                               @Arg(help = "Cell Size in meters")
                               cellSize: Distance,
                               @Arg(help = "Lower point latitude")
                               lat1: Double = -61.0,
                               @Arg(help = "Lower point longitude")
                               lng1: Double = -131.0,
                               @Arg(help = "Higher point latitude (override diag & angle)")
                               lat2: Option[Double],
                               @Arg(help = "Higher point latitude (override diag & angle)")
                               lng2: Option[Double]
                             )

case class MatMatchingKSetsOut(
                                @Arg(help = "Matches between users") matches: Map[String, String],
                                @Arg(help = "K Anonymity set size") klevel: Map[String, Int],
                                @Arg(help = "Distances measures of all re-identifications") distances: Map[String, Map[String, Double]],
                                @Arg(help = "Probability measures of all re-identifications") probabilities: Map[String, Map[String, Double]],

                                @Arg(help = "Re-Ident rate") rate: Double
                              )




