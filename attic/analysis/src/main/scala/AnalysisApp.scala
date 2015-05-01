package io.bamos.snowglobe

import org.apache.spark.{SparkConf,SparkContext}
import SparkContext._

import java.io.{File,PrintWriter}

import scala.collection.immutable.ListMap

// Akka and Spray
import akka.actor.{ActorSystem, Props}
import akka.io.IO
import spray.can.Http

object AnalysisApp {
	def main(args: Array[String]) {
    implicit val system = ActorSystem.create("SnowGlobeAnalysis")
    val handler = system.actorOf(
      Props(classOf[SnowGlobeServiceActor]),
      name = "handler"
    )

    IO(Http) ! Http.Bind(handler, interface="0.0.0.0", port=8585)
  }
}
