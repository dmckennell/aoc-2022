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
      linesFor(Day.`1`, Part.sample).use { lines =>
        val calories = lines ++ List("")
        IO.println(getHighestTotal(calories))
      }
    }

    "part a" in {
      linesFor(Day.`1`, Part.real).use { lines =>
        val calories = lines ++ List("")
        IO.println(getHighestTotal(calories))
      }
    }

    "part b" in {
      linesFor(Day.`1`, Part.real).use { lines =>
        val calories = lines ++ List("")
        val (_, highest3) = calories.foldLeft((0, List(0, 0, 0))) { case ((acc, highest3), current) =>
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

  "Day 02" - {

    sealed trait Choice {
      val value: Int
    }
    case object Rock extends Choice {
      override val value: Int = 1
    }
    case object Paper extends Choice {
      override val value: Int = 2
    }
    case object Scissors extends Choice {
      override val value: Int = 3
    }

    sealed trait Outcome {
      val score: Int
    }
    case object Win extends Outcome {
      override val score: Int = 6
    }
    case object Draw extends Outcome {
      override val score: Int = 3
    }
    case object Loss extends Outcome {
      override val score: Int = 0
    }

    val winsAgainst: Map[Choice, Choice] = Map(
      Rock     -> Paper,
      Scissors -> Rock,
      Paper    -> Scissors
    )
    val losesAgainst = winsAgainst.map(_.swap)

    object PartA {

      def determineChoice(letter: Char): Choice =
        letter match {
          case 'X' | 'A' => Rock
          case 'Y' | 'B' => Paper
          case 'Z' | 'C' => Scissors
        }

      def determineMyOutcome(me: Choice, opponent: Choice): Outcome = {
        (me, opponent) match {
          case (_, _) if winsAgainst(opponent) == me => Win
          case (_, _) if me == opponent              => Draw
          case _                                     => Loss
        }
      }

      def solve(lines: List[String]): Int =
        lines.map { game =>
          val Array(opponent, me)        = game.split(" ").take(2)
          val (opponentChoice, myChoice) = (determineChoice(opponent.charAt(0)), determineChoice(me.charAt(0)))
          val roundScore                 = determineMyOutcome(myChoice, opponentChoice).score
          roundScore + myChoice.value
        }.sum
    }

    object PartB {
      def determineOpponentChoice(letter: Char): Choice =
        letter match {
          case 'A' => Rock
          case 'B' => Paper
          case 'C' => Scissors
        }

      def determineOutcome(letter: Char): Outcome =
        letter match {
          case 'X' => Loss
          case 'Y' => Draw
          case 'Z' => Win
        }

      def determineMyChoice(outcome: Outcome, opponent: Choice): Choice =
        outcome match {
          case Win  => winsAgainst(opponent)
          case Draw => opponent
          case Loss => losesAgainst(opponent)
        }

      def solve(lines: List[String]): Int = lines.map { game =>
        val Array(opponent, result) = game.split(" ").take(2)
        val opponentChoice          = determineOpponentChoice(opponent.charAt(0))
        val outcome                 = determineOutcome(result.charAt(0))
        val myChoice                = determineMyChoice(outcome, opponentChoice)

        val roundScore = outcome.score
        roundScore + myChoice.value
      }.sum
    }

    "sample part a" in {
      linesFor(Day.`2`, Part.sample).use { lines =>
        IO.println(PartA.solve(lines))
      }
    }

    "part a" in {
      linesFor(Day.`2`, Part.real).use { lines =>
        IO.println(PartA.solve(lines))
      }
    }

    "sample part b" in {
      linesFor(Day.`2`, Part.sample).use { lines =>
        IO.println(PartB.solve(lines))
      }
    }

    "part b" in {
      linesFor(Day.`2`, Part.real).use { lines =>
        IO.println(PartB.solve(lines))
      }
    }
  }

}
