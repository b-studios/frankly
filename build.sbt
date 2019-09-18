lazy val dottyVersion = dottyLatestNightlyBuild.get

lazy val root = project
  .in(file("."))
  .settings(moduleName := "frankly", name := "frankly")
  .settings(Seq(
    scalaVersion := dottyVersion,
    version := "0.1-SNAPSHOT",
    organization := "de.b-studios",
    scalacOptions ++= Seq(
      "-deprecation",
      "-encoding", "UTF-8",
      "-feature",
      "-unchecked"
  )))



