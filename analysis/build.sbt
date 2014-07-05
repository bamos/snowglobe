name := "SnowGlobe Analysis"

version := "1.0"

scalaVersion := "2.10.4"

libraryDependencies ++= Seq(
  "org.apache.spark" %% "spark-core" % "1.0.0",
  "org.slf4j" % "slf4j-simple" % "1.7.7"
)

resolvers += "Akka Repository" at "http://repo.akka.io/releases/"
