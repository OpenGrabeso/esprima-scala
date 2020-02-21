name := "esprimascala"

version := "0.1.4"

organization := "com.github.opengrabeso"

scalaVersion := "2.12.10"

crossScalaVersions := Seq("2.12.10", "2.11.12", "2.13.1")

libraryDependencies += "org.scala-lang" % "scala-reflect" % scalaVersion.value

libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.8" % "test"

scalacOptions := Seq("-unchecked", "-deprecation", "-feature")

publishMavenStyle := true

publishArtifact in (Compile, packageDoc) := false

publish := (publish dependsOn (test in Test)).value

githubOwner := "OpenGrabeso"

githubRepository := "esprima-scala"

githubTokenSource := TokenSource.Environment("GITHUB_TOKEN") || TokenSource.GitConfig("github.token")
