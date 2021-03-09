import sbtcrossproject.CrossPlugin.autoImport.{CrossType, crossProject}
import sbtcrossproject.Platform

githubOwner in ThisBuild := "OpenGrabeso"

githubRepository in ThisBuild := "packages"

githubActor in ThisBuild := sys.env.getOrElse("GITHUB_USERNAME", "OpenGrabeso")

githubTokenSource in ThisBuild := TokenSource.GitConfig("github.token") || TokenSource.Environment("GITHUB_USERTOKEN") || TokenSource.Environment("GITHUB_TOKEN")

publish / skip := true

publishLocal / skip := true

lazy val projs = crossProject(JSPlatform, JVMPlatform).crossType(CrossType.Full).in(file("."))
  .settings(
    name := "esprimascala",
    version := "0.2.4",
    organization := "com.github.opengrabeso",

    scalaVersion := "2.12.12",
    scalacOptions := Seq("-unchecked", "-deprecation", "-feature"),
    crossScalaVersions := Seq("2.12.12", "2.11.12", "2.13.3"),

    libraryDependencies += "org.scalatest" %%% "scalatest" % "3.2.2" % "test",

    publishMavenStyle := true,
    publishArtifact in (Compile, packageDoc) := false,
    publish := (publish dependsOn (test in Test)).value,

)

lazy val root = project.in(file("root")).
  aggregate(projs.js, projs.jvm).
  settings(
    name := "esprima-scala",
    publish / skip := true,
    publishLocal / skip := true
  )