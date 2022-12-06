package org.github.dmckennell

import cats.effect.*

import scala.io.Source

object Ops:

  enum Input:
    case sample, real

  enum Part:
    case a, b

  enum Day:
    case `1`, `2`, `3`, `4`, `5`, `6`

  private def extractInputFromSource[A](day: Day, input: Input, part: Part)(f: Source => A): Resource[IO, A] =
    val source = Source.fromFile(s"./input/${day.toString}/${input.toString}.txt")
    Resource.make {
      IO.println(s"Opening file for day ${day.toString} part ${part.toString} with ${input.toString} input") *>
        IO(f(source))
    } { _ =>
      IO.println(s"Closing file for day ${day.toString} part ${part.toString} with ${input.toString} input") *>
        IO(source.close())
    }

  def linesFor(day: Day, input: Input, part: Part): Resource[IO, List[String]] =
    extractInputFromSource(day, input, part)(_.getLines.toList)

  def inputStringFor(day: Day, input: Input, part: Part): Resource[IO, String] =
    extractInputFromSource(day, input, part)(_.mkString)

  def timed(f: => IO[Unit]): IO[Unit] =
    for
      start  <- IO(System.nanoTime())
      result <- f
      finish <- IO(System.nanoTime())
    yield
      val timeTakenMs = BigDecimal((finish - start) / Math.pow(10, 6)).setScale(2, BigDecimal.RoundingMode.UP)
      println(s"took $timeTakenMs ms")
      result
