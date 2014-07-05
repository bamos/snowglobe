package io.bamos.snowglobe

// Spark.
import org.apache.spark.{SparkConf,SparkContext}
import org.apache.spark.SparkContext._
import org.apache.spark.storage.StorageLevel
import org.apache.spark.rdd.RDD

// Spray
import spray.http.{Uri,Timedout,HttpRequest,HttpResponse}
import spray.routing.HttpService

object QueryHandler {
  private val sc = createSparkContext()

  def visitors(cols: Seq[String]): String = this.synchronized {
    val sb = new StringBuilder()
    val events = sc.textFile("../events.tsv").flatMap(
      Helper.tsvToCanonicalOutput(_)
    )
    // val queryToRun = QueryMeta.info(name)
    // val queryResult = queryToRun.run(sc,data)
    // val queryResultCollected = queryResult.take(10).map(_.toString)
    html.visitors(events.collect(), cols).toString
  }

  def createSparkContext(): SparkContext = {
    val conf = new SparkConf()
      .setMaster("local[4]")
      .setAppName("SnowGlobeQuery")
      .setJars(Seq())
    new SparkContext(conf)
  }
}
