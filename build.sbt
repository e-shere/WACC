Global / onChangedBuildSource := ReloadOnSourceChanges

val projectName = "WACC_16"
val PARSLEY_VER = "3.3.2"
val SCALA_VER = "2.13.8"

lazy val root = (project in file("."))
    .settings(
        name := projectName,
        organization := "uk.ac.imperial.doc",
        scalaVersion := SCALA_VER,

        libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.10" % Test,
        libraryDependencies += "com.github.j-mie6" %% "parsley" % PARSLEY_VER,

        scalacOptions ++= Seq("-deprecation", "-unchecked", "-feature")
   )
