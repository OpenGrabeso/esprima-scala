name := "esprimaScala"

version := "0.1"

scalaVersion := "2.12.4"

libraryDependencies += "org.scala-lang" % "scala-reflect" % scalaVersion.value

libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.4" % "test"

scalacOptions := Seq("-unchecked", "-deprecation", "-feature")

