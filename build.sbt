import sbtcrossproject.CrossPlugin.autoImport.{CrossType, crossProject}

ThisBuild / githubOwner := "OpenGrabeso"

ThisBuild / githubRepository := "packages"

ThisBuild / githubActor := sys.env.getOrElse("GITHUB_USERNAME", "OpenGrabeso")

ThisBuild / githubTokenSource := TokenSource.GitConfig("github.token") || TokenSource.Environment("GITHUB_USERTOKEN") || TokenSource.Environment("GITHUB_TOKEN")

publish / skip := true

publishLocal / skip := true

lazy val projs = crossProject(JSPlatform, JVMPlatform).crossType(CrossType.Full).in(file("."))
  .settings(
    name := "esprimascala",
    version := "0.2.5",
    organization := "com.github.opengrabeso",

    scalaVersion := "2.13.6",
    scalacOptions := Seq("-unchecked", "-deprecation", "-feature"),
    crossScalaVersions := Seq("2.12.14", "2.11.12", "2.13.6"),

    libraryDependencies += "org.scalatest" %%% "scalatest" % "3.2.2" % "test",

    publishMavenStyle := true,
    Compile / packageDoc / publishArtifact := false,
    publish := (publish dependsOn (Test / test)).value,

)

lazy val root = project.in(file("root")).
  aggregate(projs.js, projs.jvm).
  settings(
    name := "esprima-scala",
    publish / skip := true,
    publishLocal / skip := true
  )