Global / onChangedBuildSource := ReloadOnSourceChanges

val projectName = "waccCompiler"
val PARSLEY_VER = "3.3.5"
val SCALA_VER = "2.13.8"

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
        scalaVersion := SCALA_VER,

		sbtAssemblySettings,

        libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.10" % Test,
        libraryDependencies += "com.github.j-mie6" %% "parsley" % PARSLEY_VER,

        scalacOptions ++= Seq("-deprecation", "-unchecked", "-feature")
   )
