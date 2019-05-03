/*
 * Copyright LIRIS-CNRS (2016)
 * Contributors: Vincent Primault <vincent.primault@liris.cnrs.fr>
 *
 * This software is a computer program whose purpose is to study location privacy.
 *
 * This software is governed by the CeCILL-B license under French law and
 * abiding by the rules of distribution of free software. You can use,
 * modify and/ or redistribute the software under the terms of the CeCILL-B
 * license as circulated by CEA, CNRS and INRIA at the following URL
 * "http://www.cecill.info".
 *
 * As a counterpart to the access to the source code and rights to copy,
 * modify and redistribute granted by the license, users are provided only
 * with a limited warranty and the software's author, the holder of the
 * economic rights, and the successive licensors have only limited liability.
 *
 * In this respect, the user's attention is drawn to the risks associated
 * with loading, using, modifying and/or developing or reproducing the
 * software by the user in light of its specific status of free software,
 * that may mean that it is complicated to manipulate, and that also
 * therefore means that it is reserved for developers and experienced
 * professionals having in-depth computer knowledge. Users are therefore
 * encouraged to load and test the software's suitability as regards their
 * requirements in conditions enabling the security of their systems and/or
 * data to be ensured and, more generally, to use and operate it in the
 * same conditions as regards security.
 *
 * The fact that you are presently reading this means that you have had
 * knowledge of the CeCILL-B license and that you accept its terms.
 */

package fr.cnrs.liris.privamov.ops

import java.io.{BufferedWriter, File, FileWriter}

import com.github.nscala_time.time.Imports._
import com.google.inject.Inject
import fr.cnrs.liris.accio.core.api._
import fr.cnrs.liris.common.geo.{Distance, Point}
import fr.cnrs.liris.common.stats.AggregatedStats
import fr.cnrs.liris.common.util.Requirements._
import fr.cnrs.liris.privamov.core.model.Trace
import fr.cnrs.liris.privamov.core.sparkle.SparkleEnv
import org.joda.time.Instant

import sys.process._

@Op(
  category = "metric",
  help = "Compute temporal distortion difference between two datasets of traces")
class PoiOpenStreetMapDistortionOp  extends Operator[PoiOpenStreetMapDistortionIn, PoiOpenStreetMapDistortionOut] with SparkleOperator {


  override def execute(in : PoiOpenStreetMapDistortionIn,ctx: OpContext): PoiOpenStreetMapDistortionOut = {
    val trainDs = read[Trace](in.train)
    val testDs = read[Trace](in.test)

    // var metrics = Map[String,AggregatedStats]

    val path = preparePOIextractionScript(ctx)
    val metrics = trainDs.zip(testDs).map { case (ref, res) => evaluate(ref, res,in,path) }.toArray
    // val listofUser = trainDs.keys.flatMap{  u => testDs.keys.flatMap{  v => if(trimUser(v)==trimUser(u)) Some((trimUser(u),u,v)) else None } }

    //  val t = trainDs.load("002")


    //  val metrics = listofUser.par.map{case (realUser, trainID, testID) => evaluate(trainDs.load(trainID).next(),testDs.load(testID).next())}


    PoiOpenStreetMapDistortionOut(
      min = metrics.map { case (k, v) => k -> v.min }.seq.toMap,
      max = metrics.map { case (k, v) => k -> v.max }.seq.toMap,
      stddev = metrics.map { case (k, v) => k -> v.stddev }.seq.toMap,
      avg = metrics.map { case (k, v) => k -> v.avg }.seq.toMap,
      median = metrics.map { case (k, v) => k -> v.median }.seq.toMap)
  }

  private def evaluate(ref: Trace, res: Trace,in : PoiOpenStreetMapDistortionIn, path : String) = {
    requireState(trimUser(ref.id) == trimUser(res.id), s"Trace mismatch: ${ref.id} / ${res.id}")
    println(s"user : ${ref.id}")
    //val (larger, smaller) = if (ref.size > res.size) (ref, res) else (res, ref)
    val distances = res.events.par.map { rec =>
      val pointInRef = interpolate(ref, rec.time)
      val pointInRes = rec.point
      comparePOIsOpenStreetMap(ref.id,pointInRef,pointInRes,in,path)
    }.seq
    ref.id -> AggregatedStats(distances)
  }

  private def interpolate(trace: Trace, time: Instant) = {
    if (time.isBefore(trace.events.head.time)) {
      trace.events.head.point
    } else if (time.isAfter(trace.events.last.time)) {
      trace.events.last.point
    } else {
      val between = trace.events.sliding(2).find { recs =>
        time.compareTo(recs.head.time) >= 0 && time.compareTo(recs.last.time) <= 0
      }.get
      if (time == between.head.time) {
        between.head.point
      } else if (time == between.last.time) {
        between.last.point
      } else {
        val ratio = (between.head.time to time).millis.toDouble / (between.head.time to between.last.time).millis
        between.head.point.interpolate(between.last.point, ratio)
      }
    }
  }

  def trimUser(s: String): String  = {
    try {
      s.toInt.toString
    } catch {
      case e: Exception => s
    }
  }

  def comparePOIsOpenStreetMap(id : String , ref : Point, res : Point ,in : PoiOpenStreetMapDistortionIn,path : String) : Double = {

    try {
      val (lat_min, lat_max, lon_min, lon_max) = box(ref, in.size)
      val reqref = s"python $path/extractPOIarea.py $lat_min $lat_max $lon_min $lon_max"
      val refOutPut = reqref !!
      val refPois = refOutPut.split(';')(1).split('\n').toSet

      val (lat2_min, lat2_max, lon2_min, lon2_max) = box(res, in.size)

      val reqres = s"python $path/extractPOIarea.py $lat2_min $lat2_max $lon2_min $lon2_max"
      val resOutPut = reqres !!
      val resPois = resOutPut.split(';')(1).split('\n').toSet
      val matches = refPois.intersect(resPois)
      MetricUtils.fscore(refPois.size, resPois.size, matches.size)
    }catch{
      case e : Exception  => 0.0
    }
  }

private def box(p : Point, d : Distance) ={
  val top = Point(p.x + d.meters , p.y + d.meters).toLatLng
  val bottom = Point(p.x - d.meters , p.y - d.meters).toLatLng
  (bottom.lat.degrees(),top.lat.degrees(),bottom.lng.degrees(),top.lng.degrees())
}

  private def preparePOIextractionScript(ctx : OpContext) ={
    val path = ctx.workDir.toAbsolutePath.toString+"Results"
    new File(path).mkdir()
    val file = new File(path+"/extractPOIarea.py")
    val bw = new BufferedWriter(new FileWriter(file))
    val script = """import sys
                   |import os
                   |import math
                   |import numpy as np
                   |from pymongo import MongoClient
                   |lat_min = float(sys.argv[1])
                   |lat_max = float(sys.argv[2])
                   |lon_min = float(sys.argv[3])
                   |lon_max = float(sys.argv[4])
                   |client = MongoClient('mongodb://localhost:27017/')
                   |query =  { "loc" :
                   |                  { "$geoWithin" :
                   |                    { "$geometry" :
                   |                      { "type" : "Polygon" ,
                   |                        "coordinates" : [ [
                   |                                         [ float(lon_min), float(lat_min) ] ,
                   |                                         [ float(lon_min) , float(lat_max)] ,
                   |                                         [  float(lon_max) , float(lat_max) ] ,
                   |                                         [   float(lon_max), float(lat_min)],
                   |                                      	[ float(lon_min), float(lat_min) ]
                   |                                       ] ]
                   |                } } } }
                   |pois =  client.osm.poi.find(query)
                   |print(";")
                   |for n in pois :
                   |        print(str(n['_id']))
                   |""".stripMargin
    println(s" SCRIPT CONTAINS $script")
    bw.write(script+"\n")
    bw.close()
    path
  }

}




case class PoiOpenStreetMapDistortionIn(
                                         @Arg(help = "Path to the File containing the Area Data of Open Street Map (pbf file)")
                                         pbfData :  String,
                                         @Arg(help = "Bouding Box half size")
                                         size : Distance,
                                       @Arg(help = "Train dataset")
                                       train: Dataset,
                                       @Arg(help = "Test dataset")
                                       test: Dataset)

case class PoiOpenStreetMapDistortionOut(
                                        @Arg(help = "Temporal distortion min")
                                        min: Map[String, Double],
                                        @Arg(help = "Temporal distortion max")
                                        max: Map[String, Double],
                                        @Arg(help = "Temporal distortion stddev")
                                        stddev: Map[String, Double],
                                        @Arg(help = "Temporal distortion avg")
                                        avg: Map[String, Double],
                                        @Arg(help = "Temporal distortion median")
                                        median: Map[String, Double])




/*
val script = """import sys
                   |import os
                   |import math
                   |import numpy as np
                   |from lxml import etree
                   |f = sys.argv[1]
                   |scId = sys.argv[2]
                   |lat_min = float(sys.argv[3])
                   |lat_max = float(sys.argv[4])
                   |lon_min = float(sys.argv[5])
                   |lon_max = float(sys.argv[6])
                   |osmOut = f.split('.')[0]+scId+"bounded.xml"
                   |osmAmenity = f.split('.')[0]+scId+"amenity.xml"
                   |os.system("osmosis --read-pbf file="+f+" --bounding-box bottom="+str(lat_min)+" top="+str(lat_max)+" left="+str(lon_min)+" right="+str(lon_max)+" --write-xml file="+osmOut)
                   |os.system("osmfilter "+osmOut+" --keep=\"amenity=\" --keep-ways=\"amenity=\" --keep-relations=\"amenity=\" > "+osmAmenity)
                   |tree = etree.parse(osmAmenity)
                   |all_relations= tree.xpath("//relation[descendant::tag[@k=\"amenity\"]]")
                   |all_ways = tree.xpath("//way[descendant::tag[@k=\"amenity\"]]")
                   |all_nodes = tree.xpath("//node[descendant::tag[@k=\"amenity\"]]")
                   |print(";")
                   |for r in all_relations :
                   |	print(r.get("id"))
                   |for w in all_ways :
                   |	print(w.get("id"))
                   |for n in all_nodes :
                   |	print(n.get("id"))
                   |""".stripMargin
*/

