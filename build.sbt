import sbtcrossproject.CrossPlugin.autoImport.{CrossType, crossProject}
import sbtcrossproject.Platform

ThisBuild / githubOwner := "OpenGrabeso"

ThisBuild / githubRepository  := "packages"

ThisBuild / githubActor := sys.env.getOrElse("GITHUB_USERNAME", "OpenGrabeso")

ThisBuild / githubTokenSource := TokenSource.GitConfig("github.token") || TokenSource.Environment("GITHUB_USERTOKEN") || TokenSource.Environment("GITHUB_TOKEN")

lazy val root = crossProject(JSPlatform, JVMPlatform).crossType(new CrossType{
  override def projectDir(crossBase: File, platform: Platform) = CrossType.Full.projectDir(crossBase, platform)
  override def projectDir(crossBase: File, projectType: String) = crossBase / projectType // copied from deprecated CrossType.Full.projectDir
  override def sharedSrcDir(projectBase: File, conf: String) = CrossType.Pure.sharedSrcDir(projectBase, conf)
}).in(file("."))
  .settings(
    name := "esprimascala",
    version := "0.2.3",
    organization := "com.github.opengrabeso",

    scalaVersion := "2.13.16",
    scalacOptions := Seq("-unchecked", "-deprecation", "-feature"),
    crossScalaVersions := Seq("2.12.20", "2.13.16"),

    libraryDependencies += "org.scalatest" %%% "scalatest" % "3.2.19" % "test",

    publishMavenStyle := true,
    Compile / packageDoc / publishArtifact := false,
    publish := (publish dependsOn (Test / test)).value,

)
