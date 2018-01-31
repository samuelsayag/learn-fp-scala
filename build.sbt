name := "learn-fp-scala"

version := "0.1"

scalaVersion := "2.12.4"

libraryDependencies += "org.scala-lang" % "scala-reflect" % scalaVersion.value

scalacOptions in ThisBuild ++= Seq("-unchecked", "-deprecation")