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
    version := "0.2.29",
    organization := "com.github.opengrabeso",

    scalaVersion := "2.13.16",
    scalacOptions := Seq("-unchecked", "-deprecation", "-feature"),

    libraryDependencies += "org.scalatest" %%% "scalatest" % "3.2.18" % "test",
    libraryDependencies += "org.scala-lang" % "scala-reflect" % scalaVersion.value, // needed for macros

    publishMavenStyle := true,
    Compile / packageDoc / publishArtifact := false,
    publish := (publish dependsOn (Test / test)).value,

)

lazy val interactive = project.dependsOn(projs.jvm)
  .settings(
    scalaVersion := "2.13.16",
    libraryDependencies += "org.scala-lang.modules" %% "scala-swing" % "3.0.0",
    libraryDependencies += "com.fifesoft" % "rsyntaxtextarea" % "3.6.0",
    publish / skip := true,
    publishLocal / skip := true
  )

lazy val root = project.in(file("root")).
  aggregate(projs.jvm, projs.js).
  settings(
    name := "esprima-scala",
    publish / skip := true,
    publishLocal / skip := true
  )
