package io.bamos.snowglobe.common

import org.apache.spark.{SparkConf,SparkContext}
import SparkContext._

import java.io.{File,PrintWriter}

import scala.collection.immutable.ListMap

object AnalysisApp extends App {
  val conf = new SparkConf()
    .setMaster("local[4]")
    .setAppName("SnowGlobeAnalysis")
    .setJars(Seq())
  val sc = new SparkContext(conf)
  val events = sc.textFile("../events.tsv").flatMap(tsvToCanonicalOutput(_))
  events.take(1).map{r => println(r.mkString(", "))}
  sc.stop()

  def tsvToCanonicalOutput(tsv: String): Option[ListMap[String,String]] = {
    val tsvFields = tsv.split("\t")
    if (tsvFields.size != Helper.headers.size) None
    else Option(ListMap(Helper.headers.zip(tsvFields): _*))
  }
}
