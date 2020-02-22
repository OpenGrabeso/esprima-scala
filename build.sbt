import sbtcrossproject.CrossPlugin.autoImport.{CrossType, crossProject}
import sbtcrossproject.Platform

lazy val projs = crossProject(JSPlatform, JVMPlatform).crossType(new CrossType{
  override def projectDir(crossBase: File, platform: Platform) = CrossType.Full.projectDir(crossBase, platform)
  override def projectDir(crossBase: File, projectType: String) = crossBase / projectType // copied from deprecated CrossType.Full.projectDir
  override def sharedSrcDir(projectBase: File, conf: String) = CrossType.Pure.sharedSrcDir(projectBase, conf)
}).in(file("."))
  .settings(
    name := "esprimascala",
    version := "0.1.5",
    organization := "com.github.opengrabeso",

    scalaVersion := "2.12.10",
    scalacOptions := Seq("-unchecked", "-deprecation", "-feature"),
    crossScalaVersions := Seq("2.12.10", "2.11.12", "2.13.1"),

    libraryDependencies += "org.scala-lang" % "scala-reflect" % scalaVersion.value,
    libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.8" % "test",

    publishMavenStyle := true,
    publishArtifact in (Compile, packageDoc) := false,
    publish := (publish dependsOn (test in Test)).value,

    githubOwner := "OpenGrabeso",
    githubRepository := "esprima-scala",
    githubTokenSource := TokenSource.Environment("GITHUB_TOKEN") || TokenSource.GitConfig("github.token")
)
