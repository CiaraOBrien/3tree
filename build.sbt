lazy val commonSettings = Seq(
  organization := "com.rklaehn",
  scalaVersion := "3.0.0",
  scalacOptions ++= Seq(
    "-indent", "-new-syntax",
    "-Yexplicit-nulls", "-language:strictEquality", 
  ),
  libraryDependencies ++= Seq(
    "org.typelevel" %% "cats-core" % "2.6.1",
    "org.scalatest" %% "scalatest" % "3.2.9" % "test",
  ),
)

lazy val radix = project.in(file("."))
.dependsOn(sonic)
.settings(commonSettings,
  name := "radixtree3",
  version := "0.1.3",
)

lazy val sonic = project.in(file("sonic"))
.settings(commonSettings,
  name := "sonicreducer3",
  version := "0.1.3",
)