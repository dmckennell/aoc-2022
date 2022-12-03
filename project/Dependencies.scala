import sbt._

object Dependencies {

  object Versions {
    val catsCore            = "2.9.0"
    val catsEffect          = "3.4.2"
    val catsEffectScalatest = "1.5.0"
    val scalatest           = "3.2.14"
  }

  lazy val catsCore   = "org.typelevel" %% "cats-core"   % Versions.catsCore
  lazy val catsEffect = "org.typelevel" %% "cats-effect" % Versions.catsEffect

  lazy val catsEffectScalatest =
    "org.typelevel" %% "cats-effect-testing-scalatest" % Versions.catsEffectScalatest % Test

  lazy val cats = List(catsCore, catsEffect, catsEffectScalatest)

}
