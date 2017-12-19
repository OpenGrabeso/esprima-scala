name := "esprimascala"

version := "0.0.1-SNAPSHOT"

organization := "com.github.opengrabeso"

scalaVersion := "2.12.4"

crossScalaVersions := Seq("2.12.4", "2.11.12", "2.10.6")

libraryDependencies += "org.scala-lang" % "scala-reflect" % scalaVersion.value

libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.4" % "test"

scalacOptions := Seq("-unchecked", "-deprecation", "-feature")

publishMavenStyle := true

publishArtifact in (Compile, packageDoc) := false

publish := (publish dependsOn (test in Test)).value

publishTo := {
  val nexus = "https://www.gamatron.net/nexus/"
  if (isSnapshot.value)
    Some("Gamatron Snapshots Nexus" at nexus + "repository/maven-snapshots")
  else
    Some("Gamatron Releases Nexus"  at nexus + "repository/maven-releases")
}

credentials += Credentials(Path.userHome / ".ivy2" / ".credentials")
