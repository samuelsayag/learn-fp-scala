name := "learn-fp-scala"

version := "0.1"

scalaVersion := "2.12.8"

libraryDependencies += "org.scala-lang" % "scala-reflect" % scalaVersion.value
libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.5" % Test

scalacOptions in ThisBuild ++= Seq("-unchecked", "-deprecation")


cancelable in Global := true

