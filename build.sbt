ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.2.1"

lazy val root = (project in file("."))
  .settings(
    name := "aoc-2022",
    scalacOptions ++= Seq(
      "-Xfatal-warnings",
      "-implicit-recursion"
    ),
    libraryDependencies ++= Dependencies.cats ++ Seq(),
    scalafmtOnCompile := true
  )
