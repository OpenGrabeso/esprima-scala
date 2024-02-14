import sbtcrossproject.CrossPlugin.autoImport.{CrossType, crossProject}
import sbtcrossproject.Platform

ThisBuild / githubOwner := "OpenGrabeso"

ThisBuild / githubRepository  := "packages"

ThisBuild / githubActor := sys.env.getOrElse("GITHUB_USERNAME", "OpenGrabeso")

ThisBuild / githubTokenSource := TokenSource.GitConfig("github.token") || TokenSource.Environment("GITHUB_USERTOKEN") || TokenSource.Environment("GITHUB_TOKEN")

lazy val projs = crossProject(JSPlatform, JVMPlatform).crossType(new CrossType{
  override def projectDir(crossBase: File, platform: Platform) = CrossType.Full.projectDir(crossBase, platform)
  override def projectDir(crossBase: File, projectType: String) = crossBase / projectType // copied from deprecated CrossType.Full.projectDir
  override def sharedSrcDir(projectBase: File, conf: String) = CrossType.Pure.sharedSrcDir(projectBase, conf)
}).in(file("."))
  .settings(
    name := "esprimascala",
    version := "0.2.2",
    organization := "com.github.opengrabeso",

    scalaVersion := "2.12.12",
    scalacOptions := Seq("-unchecked", "-deprecation", "-feature"),
    crossScalaVersions := Seq("2.12.12", "2.11.12", "2.13.3"),

    libraryDependencies += "org.scalatest" %%% "scalatest" % "3.2.2" % "test",

    publishMavenStyle := true,
    Compile / packageDoc / publishArtifact := false,
    publish := (publish dependsOn (Test / test)).value,

)

lazy val root = project.in(file("root")).
  aggregate(projs.js, projs.jvm).
  settings(
    name := "esprima-scala",
    publish := {},
    publishLocal := {},
  )
