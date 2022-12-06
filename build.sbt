ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.3.0-RC1-bin-20221201-716d93d-NIGHTLY"

lazy val root = (project in file("."))
  .settings(
    name := "aoc-2022",
    scalacOptions ++= Seq(
      "-Xfatal-warnings",
      "-deprecation"
    ),
    libraryDependencies ++= Dependencies.cats ++ Seq()
  )
