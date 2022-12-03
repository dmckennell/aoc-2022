ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "2.13.10"

lazy val root = (project in file("."))
  .settings(
    name := "aoc-2022",
    scalacOptions ++= Seq(
      "-Xfatal-warnings",
      "-Ywarn-unused:imports",
      "-Ywarn-unused:locals",
      "-Ywarn-unused:params",
      "-Ywarn-unused:patvars",
      "-Ywarn-unused:privates",
      "-Ywarn-value-discard",
      "-deprecation",
      "-feature"
    ),
    libraryDependencies ++= Dependencies.cats ++ Seq(
      Dependencies.enumeratum
    ),
    scalafmtOnCompile := true
  )
