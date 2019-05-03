/*
 * Accio is a program whose purpose is to study location privacy.
 * Copyright (C) 2016 Vincent Primault <vincent.primault@liris.cnrs.fr>
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

package fr.cnrs.liris.privamov.ops

import java.io.File

import com.google.inject.Inject
import fr.cnrs.liris.accio.core.api._
import fr.cnrs.liris.common.geo.Distance
import fr.cnrs.liris.privamov.core.clustering.DTClusterer
import fr.cnrs.liris.privamov.core.model.{Poi, PoiSet, Trace}
import fr.cnrs.liris.privamov.core.sparkle.{DataFrame, SparkleEnv}

/**
  * Implementation of a re-identification attack using the points of interest as a discriminating information. The POIs
  * are used to model the behaviour of training users, and then extracted from the tracks of test users and compared to
  * those from the training users. The comparison here is done between set of POIs, and only the spatial information.
  *
  * Vincent Primault, Sonia Ben Mokhtar, Cédric Lauradoux and Lionel Brunie. Differentially Private
  * Location Privacy in Practice. In MOST'14.
  */
@Op(
  category = "metric",
  help = "Re-identification attack using POIs a the discriminating information.")
class PoisReidentKSetOp  extends Operator[ReidentificationKSetIn, ReidentificationKSetOut] with SparkleOperator {

  override def execute(in: ReidentificationKSetIn, ctx: OpContext): ReidentificationKSetOut = {
    val trainDs = read(in.train, env)
    val testDs = read(in.test, env)
    //val listOfUsers =    ( trainDs.keys ++ testDs.keys).toSet.map{  t : String => trimUser(t)}
    val listOfUsers = (trainDs.keys.map(getUserFromId) ++ testDs.keys.map(getUserFromId)).toSet.map(trimUser)

    val trainClusterer = new DTClusterer(in.duration, in.diameter)
    val testClusterer = new DTClusterer(in.testDuration.getOrElse(in.duration), in.testDiameter.getOrElse(in.diameter))


//    val trainPois = getPois(trainDs, trainClusterer)
 //   val testPois = getPois(testDs, testClusterer)


  /*  val trainPoisdata = getPOisDataFrame(trainDs, trainClusterer)
    val testPoisdata = getPOisDataFrame(testDs, testClusterer)


    val trainPois = getPOIsFromDataFrame(trainPoisdata)
    val testPois = getPOIsFromDataFrame(testPoisdata)
*/

  val trainPoiSet = getPOISetDataFrame(trainDs,trainClusterer)
    val testPoiSet = getPOISetDataFrame(testDs,testClusterer)

    val trainPois = trainPoiSet.toArray

    var saveMapOfPOISet = Map[String,PoiSet]()
    trainPois.foreach{
      ps =>
        val u = trimUser(getUserFromId(ps.user))
        val optset = saveMapOfPOISet.get(u)
        optset match{
          case Some(set) => {
            val newSet = PoiSet(u,set.pois++ps.pois)//.sortBy(_.start))
            synchronized(saveMapOfPOISet += u -> newSet )
          }
          case None =>   synchronized(saveMapOfPOISet += u -> ps )
        }
    }
    val newPoisSet= saveMapOfPOISet.values.toSeq
    val testPois = testPoiSet.toArray



    println(s" average nb train pois =  ${newPoisSet.map(_.pois.length).sum.toDouble/trainPois.length}")
    println(s" average nb test pois =  ${testPois.map(_.pois.length).sum.toDouble/testPois.length}")



    println(s"train pois =  ${newPoisSet.map(p => p.user -> p.pois.length).toMap}")
    println(s"test pois =  ${testPois.map(p => p.user -> p.pois.length).toMap}")


    val distances = getDistances(newPoisSet, testPois)
    val outMatches = getMatches(distances,listOfUsers)


    var matches = outMatches._1
    var klevel = outMatches._2

    //println(s"matches = $matches")

    testDs.keys.foreach{
      s =>
        if(!matches.contains(s)) matches +=  s -> "-"
        if(!klevel.contains(s)) klevel +=  s -> listOfUsers.size
    }

    val probabilities = outMatches._3
   val successRate = getSuccessRateComplete(testDs.keys.length, matches)

    //ReidentificationKSetOut(distances.map { case (k, v) => k -> v.toMap }, matches, successRate)
    print("POI-Attack user re-identified : ")
    println(matches.filter{ pair => pair._1==pair._2}.keySet)
    println(s"POI-Attack rate : $successRate")

    val path = ctx.workDir.toAbsolutePath.toString+"POIs"
    new File(path).mkdir()
    write[PoiSet](trainPoiSet, ctx,"trainPOI")
    write[PoiSet](testPoiSet, ctx,"testPOI")


    ReidentificationKSetOut(matches,klevel,distances.map( e => e._1 -> e._2.toMap ),probabilities,successRate)
  }

  def trimUser(s: String): String  = {
    try {
      s.toInt.toString
    } catch {
      case e: Exception => s
    }
  }

  def getUserFromId(id : String) : String = id.split('-')(0)

  private def getPois(data: DataFrame[Trace], clusterer: DTClusterer) = {
    val allPois = data.flatMap(clusterer.cluster(_).map(c => Poi(c.events))).toArray
    allPois.groupBy(_.id).map { case (id, pois) =>  PoiSet(id, pois) }.toSeq

  }

  private def getPOisDataFrame(data: DataFrame[Trace], clusterer: DTClusterer) =  data.flatMap(clusterer.cluster(_).map(c => Poi(c.events)))

  private def getPOIsFromDataFrame(allPois : DataFrame[Poi]) = {
    allPois.toArray.groupBy(_.id).map { case (id, pois) => PoiSet(id, pois) }.toSeq

  }

  private def getPOISetDataFrame(data: DataFrame[Trace], clusterer: DTClusterer)= {
    data.map(t => PoiSet(t.id,clusterer.cluster(t).map(c => Poi(c.events))))
  }

  private def getDistances(trainPois: Seq[PoiSet], testPois: Seq[PoiSet]) = {
    val costs = collection.mutable.Map[String, Map[String, Double]]()
    testPois.foreach { pois =>
      val distances = if (pois.nonEmpty) {
        //Compute the distance between the set of pois from the test user and the models (from the training users).
        //This will give us an association between a training user and a distance. We only keep finite distances.
        trainPois.map(model => trimUser(model.user) -> model.distance(pois).meters).filter { case (u, d) => !d.isInfinite }.toMap
      } else {
        Map[String, Double]()
      }
      costs.synchronized {
        costs += pois.user -> distances
      }
    }
    costs.toSeq.map { case (user, model) =>
      user -> model.toSeq.sortWith((left,right) =>  left._2 < right._2 || (left._2 == right._2 && right._1 == user )).map { case (u: String, d: Double) => (u, d) }
    }.toMap
  }

  private def getMatches(distances: Map[String, Seq[(String, Double)]], userFull : Set[String]) = {
    var matches: Map[String, String] = Map[String, String]()
    var klevel : Map[String, Int] = Map[String, Int]()
    var probabilites : Map[String, Map[String,Double]] = Map[String, Map[String,Double]]()

     distances.foreach{ case (testUser, res) =>
       if(res.nonEmpty) {
         var level = res.map(_._1).indexOf(trimUser(getUserFromId(testUser))) + 1
         if(level == 0) level = userFull.size
         klevel += (testUser -> level)
         matches += (testUser -> res.head._1)
       }else{
         klevel += (testUser -> userFull.size)
         matches += (testUser -> "-")
       }

    }
    probabilites = distances.map{
      case (testuser, order) =>
        if(order.nonEmpty) {
          val maxDist = order.map(_._2).max
          val minDist = order.map(_._2).min
          val sim = order.map(p => (p._1, 1 - normelize(p._2 , minDist, maxDist))).toMap
          val sumSim = sim.values.sum
          val prob = sim.map(p =>  p._1 -> p._2 / sumSim)
          testuser -> prob
        }else {
          testuser -> userFull.map(_ -> 0.0).toMap
        }
    }


    userFull.foreach { u =>
     /* if (!matches.contains(u)){
        klevel += (u -> userFull.size)
        matches += u -> "-"
        probabilites +=  u-> userFull.map(_ -> 0.0).toMap
      }*/
      probabilites = probabilites.map{
        prob =>
          val newP = if(prob._2.contains(u)) prob._2 else prob._2 + (u -> 0.0)
          prob._1 -> newP
      }
    }

      (matches,klevel,probabilites)
  }

  def normelize(value: Double, minV: Double, maxV: Double): Double = (value - minV) / (maxV - minV)


  private def getSuccessRate(trainPois: Seq[PoiSet], matches: Map[String, String]) = {
    val trainUsers = trainPois.map(_.user)
    val nbMataches = matches.map { case (testUser, trainUser) =>
      if (testUser == trainUser) {
        1
      }
      else {
        0
      }
    }.sum
    val nbUsers = trainUsers.size.toDouble
    nbMataches.toDouble / nbUsers.toDouble
  }

  private def getSuccessRateComplete(nbUsers : Int, matches: Map[String, String]) = {
    val nbMataches = matches.map { case (testUser, trainUser) =>
      if (trimUser(getUserFromId(testUser)) == trainUser) {
        1
      }
      else {
        0
      }
    }.sum
    nbMataches.toDouble / nbUsers.toDouble
  }
}

case class ReidentificationKSetIn(
                               @Arg(help = "Clustering maximum diameter")
                               diameter: Distance,
                               @Arg(help = "Clustering minimum duration")
                               duration: org.joda.time.Duration,
                               @Arg(help = "Override the clustering maximum diameter to use with the test dataset only")
                               testDiameter: Option[Distance],
                               @Arg(help = "Override the clustering minimum duration to use with the test dataset only")
                               testDuration: Option[org.joda.time.Duration],
                               @Arg(help = "Train dataset")
                               train: Dataset,
                               @Arg(help = "Test dataset")
                               test: Dataset)

case class ReidentificationKSetOut(
                                @Arg(help = "Matches between users") matches: Map[String, String],
                                @Arg(help = "K Anonymity set size") klevel: Map[String, Int],
                                @Arg(help = "Distances measures of all re-identifications") distances: Map[String, Map[String,Double]],
                                @Arg(help = "Probability measures of all re-identifications (Full informaiton") probabilities: Map[String, Map[String,Double]],
                                @Arg(help = "Re-Ident rate") rate: Double
                              )