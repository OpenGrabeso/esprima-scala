name := "esprimascala"

version := "0.0.1-SNAPSHOT"

organization := "com.github.opengrabeso"

scalaVersion := "2.11.12"

crossScalaVersions := Seq("2.12.4", "2.11.12", "2.10.6")

libraryDependencies += "org.scala-lang" % "scala-reflect" % scalaVersion.value

libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.4" % "test"

scalacOptions := Seq("-unchecked", "-deprecation", "-feature")

publishMavenStyle := true

publishArtifact in (Compile, packageDoc) := false

publish := (publish dependsOn (test in Test)).value

publishTo := {
  val sonatype = "https://oss.sonatype.org/"
  if (isSnapshot.value)
    Some("Gamatron Snapshots Nexus" at sonatype + "content/repositories/snapshots")
  else
    Some("Gamatron Releases Nexus"  at sonatype + "service/local/staging/deploy/maven2")
}

credentials += Credentials(Path.userHome / ".ivy2" / "oss.credentials")

// use %userprofile%/.ivy2 to access the folder in Windows run command