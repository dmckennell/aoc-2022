package org.github

import cats.effect._
import cats.effect.testing.scalatest.AsyncIOSpec
import org.github.Prep._
import org.scalatest.matchers.should.Matchers
import org.scalatest.freespec.AsyncFreeSpec

class AdventOfCodeTest extends AsyncFreeSpec with AsyncIOSpec with Matchers {

  "Day 01 " - {
    "part a" in {
      val file = getInputFor(Day.`1`, Part.a)
      file.use { stuff =>
        IO(println(stuff))
      }
    }
  }

}
