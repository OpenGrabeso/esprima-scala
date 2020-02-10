name := "esprimascala"

version := "0.1.3-SNAPSHOT"

organization := "com.github.opengrabeso"

scalaVersion := "2.12.10"

crossScalaVersions := Seq("2.12.10", "2.11.12", "2.13.1")

libraryDependencies += "org.scala-lang" % "scala-reflect" % scalaVersion.value

libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.8" % "test"

scalacOptions := Seq("-unchecked", "-deprecation", "-feature")

publishMavenStyle := true

publishArtifact in (Compile, packageDoc) := false

publish := (publish dependsOn (test in Test)).value

publishTo := {
  if (isSnapshot.value) {
    val sonatype = "https://oss.sonatype.org/"
    Some("Sonatues Snapshots" at sonatype + "content/repositories/snapshots")
  } else {
    // TODO: provide all which is necessary for Sonatype releases
    // see https://central.sonatype.org/pages/requirements.html
    val gamatron = "https://www.gamatron.net/nexus/"
    Some("Gamatron Releases Nexus" at gamatron + "service/local/staging/deploy/maven2")
  }
}

credentials += Credentials(Path.userHome / ".ivy2" / "oss.credentials")

// use %userprofile%/.ivy2 to access the folder in Windows run command