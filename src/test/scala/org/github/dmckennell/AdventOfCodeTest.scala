package org.github.dmckennell

import cats.effect._
import cats.effect.testing.scalatest.AsyncIOSpec
import org.github.dmckennell.Ops._
import org.scalatest.freespec.AsyncFreeSpec
import org.scalatest.matchers.should.Matchers

class AdventOfCodeTest extends AsyncFreeSpec with AsyncIOSpec with Matchers {

  "Day 01 " - {
    def getHighestTotal(input: List[String]): Int = {
      val (_, highest) = input.foldLeft((0, 0)) { case ((acc, highest), current) =>
        if (current == "") {
          if (acc > highest) (0, acc) else (0, highest)
        } else {
          (acc + current.toInt, highest)
        }
      }
      highest
    }

    "sample" in {
      linesFor(Day.`1`, Part.sample).use { calories =>
        val input = calories ++ List("")
        IO.println(getHighestTotal(input))
      }
    }

    "part a" in {
      linesFor(Day.`1`, Part.real).use { calories =>
        val input = calories ++ List("")
        IO.println(getHighestTotal(input))
      }
    }

    "part b" in {
      linesFor(Day.`1`, Part.real).use { calories =>
        val input = calories ++ List("")
        val (_, highest3) = input.foldLeft((0, List(0, 0, 0))) { case ((acc, highest3), current) =>
          if (current == "") {
            val (lowest, idx) = highest3.zipWithIndex.minBy { case (value, _) => value }
            if (acc > lowest) (0, highest3.updated(idx, acc)) else (0, highest3)
          } else {
            (acc + current.toInt, highest3)
          }
        }
        IO.println(highest3.sum)
      }
    }
  }

}
