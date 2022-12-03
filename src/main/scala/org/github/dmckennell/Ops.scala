package org.github.dmckennell

import cats.effect._
import enumeratum._

import scala.io.Source

object Ops {

  sealed trait Part extends EnumEntry

  object Part extends Enum[Part] {
    val values = findValues
    case object sample extends Part
    case object real   extends Part
  }

  sealed trait Day extends EnumEntry

  object Day extends Enum[Day] {
    val values = findValues
    case object `1` extends Day
    case object `2` extends Day
    case object `3` extends Day
  }

  def linesFor(day: Day, part: Part): Resource[IO, List[String]] = {
    val source = Source.fromFile(s"./input/${day.toString}/${part.toString}.txt")
    Resource.make(IO(source.getLines().toList)) { _ =>
      IO.println(s"Closing file for day ${day.toString}, part ${part.toString}") *> IO(source.close())
    }
  }

  def timed(f: => IO[Unit]): IO[Unit] = {
    for {
      start  <- IO(System.nanoTime())
      result <- f
      finish <- IO(System.nanoTime())
    } yield {
      val timeTakenMs = BigDecimal((finish - start) / Math.pow(10, 6)).setScale(2, BigDecimal.RoundingMode.UP)
      println(s"took $timeTakenMs ms")
      result
    }
  }

}
