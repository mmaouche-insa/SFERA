
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

import com.google.inject.Inject
import fr.cnrs.liris.accio.core.api._
import fr.cnrs.liris.common.geo._
import fr.cnrs.liris.privamov.core.model.Trace
import fr.cnrs.liris.privamov.core.sparkle.{DataFrame, SparkleEnv}
import com.google.common.geometry._
import java.io._

import fr.cnrs.liris.common.util.TimeUtils
import org.joda.time.Duration
import org.joda.time.DateTimeConstants

import scala.collection._
import scala.util.Random


@Op(
  category = "prepare",
  help = "Splits the traces based on the starting and ending time")
class TrainTestSplitSubTracesOp  extends Operator[TrainTestSplitSubTracesIn, TrainTestSplitSubTracesOut] with SparkleOperator {


  override def execute(in: TrainTestSplitSubTracesIn, ctx: OpContext): TrainTestSplitSubTracesOut = {
    // read the data
    val ds = read[Trace](in.data)
    var size = Map[String,Int]()
    ds.foreach{
      t=>
        val id = t.id.split("-")(1).toInt
        val v = size.get(t.user) match {
          case Some(value) => math.max(value,id)
          case None => id
        }
        synchronized(size += (t.user -> v))
    }

    //println(size)


    val newDs = ds.filter{
      t =>
        val id = t.id.split("-")(1).toInt

        val bool = size.get(t.user) match {
          case Some(v) => if(in.train) id <= v*in.portion else id > v*in.portion
          case None => false
        }
        //println(s"for ${t.id}  ${size.getOrElse(t.user,-1)} considering ${if(in.train) s"Train  $id <= ${size.getOrElse(t.user,-1)*in.portion}"  else s"Test $id > ${size.getOrElse(t.user,-1)*in.portion}"}  is $bool ")
        bool
    }

    TrainTestSplitSubTracesOut(write[Trace](newDs, ctx))
  }

}

case class TrainTestSplitSubTracesIn(
                                @Arg(help = "Input dataset")
                                data: Dataset,
                                @Arg(help = "Proportion of train")
                                portion: Double,
                                @Arg(help = " Train with True or Test with False")
                                train: Boolean
                              )

case class TrainTestSplitSubTracesOut(
                                 @Arg(help = "Output dataset")
                                 output: Dataset

                               )

