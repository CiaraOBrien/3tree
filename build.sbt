lazy val commonSettings = Seq(
  organization := "com.rklaehn",
  scalaVersion := "3.0.0-RC1",
  scalacOptions ++= Seq(
    "-source:future", "-indent", "-new-syntax",
    "-Yexplicit-nulls", "-Ycheck-init", "-Yerased-terms",
    "-language:strictEquality", 
  ),
  libraryDependencies ++= Seq(
    "org.typelevel" %% "cats-core" % "2.4.2",
    "org.scalatest" %% "scalatest" % "3.2.5" % "test",
  ),
)

lazy val radix = project.in(file("."))
.dependsOn(sonic)
.settings(commonSettings,
  name := "radixtree3",
  version := "0.1.0",
  
)

lazy val sonic = project.in(file("sonic"))
.settings(commonSettings,
  name := "sonicreducer3",
  version := "0.1.0",
)