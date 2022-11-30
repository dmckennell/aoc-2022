package org.github

import cats.effect._
import enumeratum._

import scala.io.Source

object Prep {

  sealed trait Part extends EnumEntry

  object Part extends Enum[Part] {
    val values = findValues
    case object a extends Part
    case object b extends Part
  }

  sealed trait Day extends EnumEntry

  object Day extends Enum[Day] {
    val values = findValues

    case object `1` extends Day
  }

  def getInputFor(day: Day, part: Part): Resource[IO, List[String]] = {
    val source = Source.fromFile(s"/Users/david/code/aoc-2022/input/${day.toString}/${part.toString}.txt")
    Resource.make(IO(source.getLines().toList)) { _ =>
      IO.println(s"Closing file for day ${day.toString}, part ${part.toString}") *> IO(source.close())
    }
  }

}
