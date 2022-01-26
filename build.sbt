Global / onChangedBuildSource := ReloadOnSourceChanges

val projectName = "WACC_16"

lazy val sbtAssemblySettings = baseAssemblySettings ++ Seq(
  assembly / assemblyOutputPath := baseDirectory.value / s"$projectName.jar",
  assembly / assemblyMergeStrategy := {
    case PathList("META-INF", xs @ _*) => MergeStrategy.discard
    case _                             => MergeStrategy.first
  }
)

lazy val root = (project in file("."))
    .settings(
        name := projectName,
        organization := "uk.ac.imperial.doc",
        scalaVersion := "2.13.7",

        sbtAssemblySettings,

        libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.10" % Test,

        scalacOptions ++= Seq("-deprecation", "-unchecked", "-feature")
   )
