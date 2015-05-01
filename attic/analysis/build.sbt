import com.typesafe.sbt.SbtStartScript

Revolver.settings

name := "SnowGlobe Analysis"

version := "1.0"

scalaVersion := "2.10.4"

unmanagedResourceDirectories in Compile <<= Seq(
  baseDirectory / "src/main/webapp"
).join

libraryDependencies ++= Seq(
  "org.apache.spark" %% "spark-core" % "1.0.0",
  // "org.slf4j" % "slf4j-simple" % "1.7.7",
  "io.spray" % "spray-can" % "1.2.1",
  "io.spray" % "spray-routing" % "1.2.1",
  "com.typesafe.akka" %% "akka-actor" % "2.2.3",
  "com.typesafe.akka" %% "akka-slf4j" % "2.2.3"
)

resolvers ++= Seq(
  "Akka Repository" at "http://repo.akka.io/releases/",
  "Spray repo" at "http://repo.spray.io"
)

lazy val root = (project in file(".")).enablePlugins(SbtTwirl)

seq(SbtStartScript.startScriptForClassesSettings: _*)
