lazy val dottyVersion = "0.17.0-bin-20190711-e2130b9-NIGHTLY" // dottyLatestNightlyBuild.get

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



