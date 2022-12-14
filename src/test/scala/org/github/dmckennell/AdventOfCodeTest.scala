package org.github.dmckennell

import cats.effect.*
import cats.effect.testing.scalatest.AsyncIOSpec
import cats.implicits.*
import org.github.dmckennell.Ops.*
import org.scalatest.freespec.AsyncFreeSpec
import org.scalatest.matchers.should.Matchers
import language.experimental.fewerBraces
import scala.annotation.tailrec

class AdventOfCodeTest extends AsyncFreeSpec with AsyncIOSpec with Matchers:

  "Day 01" - {
    def getHighestTotal(input: List[String]): Int =
      val (_, highest) = input.foldLeft((0, 0)) { case ((acc, highest), current) =>
        if (current == "")
          if (acc > highest) (0, acc) else (0, highest)
        else
          (acc + current.toInt, highest)
      }
      highest

    "sample" in:
      linesFor(Day.`1`, Input.sample, Part.a).use: lines =>
        val calories = lines ++ List("")
        IO.println(getHighestTotal(calories))

    "part a" in:
      linesFor(Day.`1`, Input.real, Part.a).use: lines =>
        val calories = lines ++ List("")
        IO.println(getHighestTotal(calories))

    "part b" in:
      linesFor(Day.`1`, Input.real, Part.b).use: lines => 
        val calories = lines ++ List("")
        val (_, highest3) = calories.foldLeft((0, List(0, 0, 0))) { case ((acc, highest3), current) =>
          if (current == "")
            val (lowest, idx) = highest3.zipWithIndex.minBy { case (value, _) => value }
            if (acc > lowest) (0, highest3.updated(idx, acc)) else (0, highest3)
          else
            (acc + current.toInt, highest3)
        }
        IO.println(highest3.sum)
  }

  "Day 02" - {
    enum Choice(val value: Int):
      case Rock     extends Choice(1)
      case Paper    extends Choice(2)
      case Scissors extends Choice(3)

    enum Outcome(val score: Int):
      case Win  extends Outcome(6)
      case Draw extends Outcome(3)
      case Loss extends Outcome(0) 

    val winsAgainst: Map[Choice, Choice] = Map(
      Choice.Rock     -> Choice.Paper,
      Choice.Scissors -> Choice.Rock,
      Choice.Paper    -> Choice.Scissors
    )
    val losesAgainst = winsAgainst.map(_.swap)

    object PartA:
      def determineChoice(letter: Char): Choice = 
        letter match
          case 'X' | 'A' => Choice.Rock
          case 'Y' | 'B' => Choice.Paper
          case 'Z' | 'C' => Choice.Scissors

      def determineMyOutcome(me: Choice, opponent: Choice): Outcome =
        (me, opponent) match
          case (_, _) if winsAgainst(opponent) == me => Outcome.Win
          case (_, _) if me == opponent              => Outcome.Draw
          case _                                     => Outcome.Loss

      def solve(lines: List[String]): Int = 
        lines.map: game =>
          val Array(opponent, me)        = game.split(" ").take(2)
          val (opponentChoice, myChoice) = (determineChoice(opponent.charAt(0)), determineChoice(me.charAt(0)))

          determineMyOutcome(myChoice, opponentChoice).score + myChoice.value
        .sum

    object PartB:
      def determineOpponentChoice(letter: Char): Choice =
        letter match
          case 'A' => Choice.Rock
          case 'B' => Choice.Paper
          case 'C' => Choice.Scissors

      def determineOutcome(letter: Char): Outcome =
        letter match
          case 'X' => Outcome.Loss
          case 'Y' => Outcome.Draw
          case 'Z' => Outcome.Win

      def determineMyChoice(outcome: Outcome, opponent: Choice): Choice =
        outcome match
          case Outcome.Win  => winsAgainst(opponent)
          case Outcome.Draw => opponent
          case Outcome.Loss => losesAgainst(opponent)

      def solve(lines: List[String]): Int = 
        lines.map: game =>
          val Array(opponent, result) = game.split(" ").take(2)
          val opponentChoice          = determineOpponentChoice(opponent.charAt(0))
          val outcome                 = determineOutcome(result.charAt(0))
          val myChoice                = determineMyChoice(outcome, opponentChoice)

          outcome.score + myChoice.value
        .sum

    "sample part a" in:
      linesFor(Day.`2`, Input.sample, Part.a).use: lines =>
        IO.println(PartA.solve(lines))

    "part a" in:
      linesFor(Day.`2`, Input.real, Part.a).use: lines =>
        IO.println(PartA.solve(lines))

    "sample part b" in:
      linesFor(Day.`2`, Input.sample, Part.b).use: lines =>
        IO.println(PartB.solve(lines))

    "part b" in:
      linesFor(Day.`2`, Input.real, Part.b).use: lines =>
        IO.println(PartB.solve(lines))
  }

  "Day 03" - {
    def getScore(character: Char): Int =
      character match 
        case c if c.isUpper => c.toInt - 38
        case c              => c.toInt - 96 // must be lowercase

    object PartA:
      def splitCompartments(rucksack: String): (String, String) =
        val (first, second) = rucksack.splitAt(rucksack.length / 2)
        (first.distinct, second.distinct)

      def findDuplicate(firstCompartment: String, secondCompartment: String): Option[Char] =
        firstCompartment.collectFirst {
          case item if secondCompartment.contains(item) => item
        }

      def solve(rucksacks: List[String]): Int =
        rucksacks.map(splitCompartments).map: (first, second) =>
          findDuplicate(first, second).fold(0)(getScore)
        .sum

    object PartB:
      def groupElves(rucksacks: List[String]): List[List[String]] =
        rucksacks.grouped(3).toList

      def findTriplicate(first: String, second: String, third: String): Option[Char] =
        first.collectFirst {
          case item if second.contains(item) && third.contains(item) => item
        }

      def solve(rucksacks: List[String]): Int =
        groupElves(rucksacks).map {
          case List(first, second, third) => findTriplicate(first, second, third).fold(0)(getScore)
          case _                          => fail()
        }.sum

    "sample part a" in:
      linesFor(Day.`3`, Input.sample, Part.a).use: lines =>
        IO.println(PartA.solve(lines))

    "part a" in:
      linesFor(Day.`3`, Input.real, Part.a).use: lines =>
        IO.println(PartA.solve(lines))

    "sample part b" in:
      linesFor(Day.`3`, Input.sample, Part.b).use: lines =>
        IO.println(PartB.solve(lines))

    "part b" in:
      linesFor(Day.`3`, Input.real, Part.b).use: lines =>
        timed:
          IO.println(PartB.solve(lines))
  }

  "Day 4" - {
    case class Section(begin: Int, end: Int)

    def getElfPairs(assignmentInput: String): (Section, Section) =
      assignmentInput.split(",") match
        case Array(first, second) =>
          val Array(beginFirst, endFirst)   = first.split("-")
          val Array(beginSecond, endSecond) = second.split("-")
          (Section(beginFirst.toInt, endFirst.toInt), Section(beginSecond.toInt, endSecond.toInt))

    object PartA:
      def sectionContainsOther(thisSection: Section, thatSection: Section): Boolean =
        (thisSection.begin <= thatSection.begin && thisSection.end >= thatSection.end)
        ||
        (thatSection.begin <= thisSection.begin && thatSection.end >= thisSection.end)

    object PartB:
      def sectionOverlapsOther(thisSection: Section, thatSection: Section): Boolean =
        !((thisSection.end < thatSection.begin) || (thatSection.end < thisSection.begin))

    "sample part a" in:
      linesFor(Day.`4`, Input.sample, Part.a).use: lines =>
        IO.println(lines.map(getElfPairs).count(PartA.sectionContainsOther))

    "part a" in:
      linesFor(Day.`4`, Input.real, Part.a).use: lines =>
        IO.println(lines.map(getElfPairs).count(PartA.sectionContainsOther))

    "sample part b" in:
      linesFor(Day.`4`, Input.sample, Part.b).use: lines =>
        IO.println(lines.map(getElfPairs).count(PartB.sectionOverlapsOther))

    "part b" in:
      linesFor(Day.`4`, Input.real, Part.b).use: lines =>
        IO.println(lines.map(getElfPairs).count(PartB.sectionOverlapsOther))
  }

  "Day 5" - {
    enum CraneMoverModel:
      case `9000`, `9001`

    case class Instruction(from: Int, to: Int, number: Int)

    val crateRegex = """\[([A-Z-])\]""".r
    val instructionRegex = """^move\s(\d+)\sfrom\s(\d+)\sto\s(\d+)$""".r

    def gatherInstructions(instructionsInput: String): Array[Instruction] =
      instructionsInput.split("\\n").map {
        case instructionRegex(number, from, to) => Instruction(from.toInt, to.toInt, number.toInt)
      }

    def gatherCrates(cratesInput: String): Map[Int, Array[Char]] =
      cratesInput.split("\\n")
        .map: cratesSlice =>
          crateRegex.findAllMatchIn(cratesSlice).map(_.group(1)).toArray
        .transpose
        .map: crates =>
          crates
            .reverse
            .filterNot(_ == "-") // input blank crates have been changed to hyphens for convenience
            .map(_.charAt(0))
        .zipWithIndex.map: (crateArray, idx) =>
          (idx + 1, crateArray)
        .toMap

    /*
        Following modifications made to input:
          - stack number line removed
          - empty crates have been relabelled as [-]
    */
    def parseInput(input: String): (Array[Instruction], Map[Int, Array[Char]]) =
      val Array(crateInfo, instructionInfo) = input.split("\\n\\n")
      (gatherInstructions(instructionInfo), gatherCrates(crateInfo))
      
    def solve(input: String, craneMoverModel: CraneMoverModel): String =
      val (instructions, cratesMap) = parseInput(input)
      instructions.foldLeft(cratesMap): (currentStacks, instruction) =>
        val from        = currentStacks(instruction.from)
        val to          = currentStacks(instruction.to)
        val valuesToAdd = from.takeRight(instruction.number)
        val add         = craneMoverModel match
          case CraneMoverModel.`9000` => valuesToAdd.reverse
          case CraneMoverModel.`9001` => valuesToAdd
        currentStacks
          .updated(instruction.from, from.dropRight(instruction.number))
          .updated(instruction.to, to ++: add)
      .toArray
      .sortBy: (idx, _) =>
        idx
      .flatMap: (_, crates) =>
        crates.lastOption
      .mkString

    "sample part a" in:
      inputStringFor(Day.`5`, Input.sample, Part.a).use: input =>
        IO.println(solve(input, CraneMoverModel.`9000`))

    "part a" in :
      inputStringFor(Day.`5`, Input.real, Part.a).use: input =>
        IO.println(solve(input, CraneMoverModel.`9000`))
    
    "sample part b" in:
      inputStringFor(Day.`5`, Input.sample, Part.b).use: input =>
        IO.println(solve(input, CraneMoverModel.`9001`))

    "part b" in :
      inputStringFor(Day.`5`, Input.real, Part.b).use: input =>
        timed:
          IO.println(solve(input, CraneMoverModel.`9001`))
  }

  "Day 6" - {
    def findFirstMarker(input: String, distinctCount: Int): Int =
      @tailrec
      def markerIndexAtEndOfUniqueCharSequence(buffer: Array[Char], idx: Int = 0): Int =
        val current = input(idx)
        if (buffer.size == (distinctCount - 1) && !buffer.contains(current))
          idx
        else
          val (_, prunedBuffer) = buffer.splitAt(buffer.indexOf(current) + 1)
          markerIndexAtEndOfUniqueCharSequence(prunedBuffer :+ current, idx + 1)
      markerIndexAtEndOfUniqueCharSequence(Array.emptyCharArray, 0) + 1

    "sample part a" in:
      linesFor(Day.`6`, Input.sample, Part.a).use: examples =>
        IO.println(examples.map(findFirstMarker(_, 4)).mkString(", "))
    
    "part a" in:
      inputStringFor(Day.`6`, Input.real, Part.a).use: input =>
        timed:
          IO.println(findFirstMarker(input, 4))
    
    "sample part b" in:
      linesFor(Day.`6`, Input.sample, Part.b).use: examples =>
        IO.println(examples.map(findFirstMarker(_, 14)).mkString(", "))

    "part b" in:
      inputStringFor(Day.`6`, Input.real, Part.b).use: input =>
        timed:
          IO.println(findFirstMarker(input, 14))
  }

  "Day 7" - {
    sealed trait PromptLine
    case class File(name: String, size: Long) extends PromptLine
    case class DirectoryIdentifier(name: String) extends PromptLine

    sealed trait Command extends PromptLine
    case object ListContents extends Command
    case class ChangeDirectory(to: String) extends Command
    case object GoHome extends Command

    case class Directory(name: String, files: Set[File], parentName: Option[String], allParents: Set[String])

    def parseLine(input: String): PromptLine =
      input match 
        case i if i.startsWith("$") =>
          i.drop(2).take(2) match 
            case "ls"                             => ListContents
            case "cd" if i.split(" ").last == "/" => GoHome
            case "cd"                             => ChangeDirectory(i.split(" ").last)
            case _                                => fail()
        case i if i.startsWith("dir") => DirectoryIdentifier(i.split(" ").last)
        case i =>
          val Array(size, name) = i.split(" ")
          File(name, size.toLong)
        

    def gatherFileStructure(input: List[String]): List[Directory] =
      val instructions = input.map(parseLine)
      def generateFullPathForDirectory(currentDirectory: String, newDir: String): String =        
        if (currentDirectory == "/")
          currentDirectory.dropRight(1) + newDir
        else
          currentDirectory + "/" + newDir
      
      val (initialDirectories, initialWorkingDirectory) = (Map("/" -> (Directory("/", Set.empty, None, Set.empty))), "/")

      val (directories, _) = instructions.foldLeft((initialDirectories, initialWorkingDirectory)) { case ((directories, currentDir), current) =>
        current match
          case GoHome              => (directories, "/")
          case ChangeDirectory(to) =>
            if (to == "..")
              val parent = directories(currentDir).parentName.get
              (directories, parent)
            else
              val fullPath = generateFullPathForDirectory(currentDir, to)
              (directories, directories(fullPath).name)
          case ListContents              => (directories, currentDir)
          case DirectoryIdentifier(name) => 
            val fullPath = generateFullPathForDirectory(currentDir, name)
            directories.get(fullPath) match
              case Some(existing) => (directories, currentDir)
              case None => 
                val parent = directories(currentDir)
                (directories + (fullPath -> Directory(fullPath, Set.empty, currentDir.some, parent.allParents + parent.name)), currentDir)
          case f@File(name, size) => 
            val dir     = directories(currentDir)
            val updated = dir.copy(files = dir.files + f)
            (directories.updated(currentDir, updated), currentDir)
      }
      directories.values.toList

    def directory2Children(directories: List[Directory]): Map[String, Set[String]] =
      directories.foldLeft(Map.empty[String, Set[String]]): (acc, current) =>
        current.allParents.foldLeft(acc): (acc, p) =>
          acc.get(p) match
            case Some(parent) => acc.updated(p, (acc(p) + current.name))
            case None         => acc + (p -> Set(current.name))
    
    def directories2FileTotals(directories: List[Directory]): Map[String, Long] =
      directories.map: directory =>
        directory.name -> directory.files.map(_.size).sum
      .toMap

    def getTotals(directories: List[Directory]): List[(Directory, Long)] =
      val fileTotalsLookup    = directories2FileTotals(directories)
      val directoriesChildren = directory2Children(directories)
      directories.map: d =>
        val childNames = directoriesChildren.get(d.name).getOrElse(Set.empty)
        (d -> (childNames.map(fileTotalsLookup).sum + d.files.map(_.size).sum))

    object PartA:
      def solve(input: List[String]): Long =
        getTotals(gatherFileStructure(input)).filter: directoryDetails =>
          val (_, totalSize) = directoryDetails
          totalSize <= 100000L
        .map: directoryDetails =>
          val (_, totalSize) = directoryDetails
          totalSize
        .sum

    object PartB:
      def solve(input: List[String]): Long = 
        val results = getTotals(gatherFileStructure(input))
        val (_, rootDirectoryTotalSize) = results.find { case (directory, _) =>
          directory.name == "/"
        }.get // a bit lazy
        val totalRemaining = 70000000L - rootDirectoryTotalSize
        val required       = 30000000L - totalRemaining
        val (_, smallestCompensator) =  results.filter: directoryDetails =>
          val (_, totalSize) = directoryDetails
          totalSize >= required 
        .minBy: directoryDetails =>
          val (_, totalSize) = directoryDetails
          totalSize
        
        smallestCompensator

    "sample part a" in:
      linesFor(Day.`7`, Input.sample, Part.a).use: input =>
        timed: 
          IO.println(PartA.solve(input))
    
    "part a" in:
      linesFor(Day.`7`, Input.real, Part.a).use: input =>
        timed: 
          IO.println(PartA.solve(input))

    "sample part b" in:
      linesFor(Day.`7`, Input.sample, Part.b).use: input =>
        timed:
          IO.println(PartB.solve(input))

    "part b" in:
      linesFor(Day.`7`, Input.real, Part.b).use: input =>
        timed:
          IO.println(PartB.solve(input))
  }

  "Day 8" - {
    def generateTreeIndices(input: List[String]): List[(Int, Int)] =
      val endIdx  = input.size - 1

      (for
        i <- 0 to endIdx
        j <- 0 to endIdx
      yield (i, j)).toList
    
    def treesAndTransposition(input: List[String]): (List[List[Int]], List[List[Int]]) =
      val trees   = input.map(_.map(_.toString.toInt).toList)
      val flipped = trees.transpose
      (trees, flipped)

    object PartA:
      def getVisibles(treeIndices: List[(Int, Int)], input: List[String]): Int =
        val (trees, flipped) = treesAndTransposition(input)
        val endIdx           = input.size - 1
        
        treeIndices.map: idx =>
          val (i, j)     = idx
          val horizontal = trees(i)
          val vertical   = flipped(j)
          val tree       = horizontal(j)

          if (i == 0 || i == endIdx || j == 0 || j == endIdx)
            true
          else
            val (left, right) = 
              val (left, rightInclusive) = horizontal.splitAt(j)
              (left, rightInclusive.tail)
            val (above, below) =
              val (above, belowInclusive) = vertical.splitAt(i)
              (above, belowInclusive.tail)
            
            tree > left.max || tree > right.max
            ||
            tree > above.max || tree > below.max
        .count(identity)

      def solve(input: List[String]): Int =
        getVisibles(generateTreeIndices(input), input)
    
    object PartB:
      def scenicScore(tree: Int, neighbours: List[Int]): Int =
        @tailrec
        def count(remainingTrees: List[Int], currentCount: Int = 0): Int =
          remainingTrees match
            case Nil => currentCount
            case head :: tail =>
              if (head < tree)
                count(tail, currentCount + 1)
              else currentCount + 1
        count(neighbours)

      def highestScenicScore(treeIndices: List[(Int, Int)], input: List[String]): Int = 
        val (trees, flipped) = treesAndTransposition(input)
        
        treeIndices.map: idx =>
          val (i, j)     = idx
          val horizontal = trees(i)
          val vertical   = flipped(j)
          val tree       = horizontal(j)
          val (left, right) = 
            val (left, rightInclusive) = horizontal.splitAt(j)
            (left, rightInclusive.tail)
          val (above, below) =
            val (above, belowInclusive) = vertical.splitAt(i)
            (above, belowInclusive.tail)

          scenicScore(tree, left.reverse) * scenicScore(tree, right) * scenicScore(tree, above.reverse) * scenicScore(tree, below)
        .max
      
      def solve(input: List[String]): Int =
        highestScenicScore(generateTreeIndices(input), input)

    "sample part a" in:
      linesFor(Day.`8`, Input.sample, Part.a).use: input =>
        IO.println(PartA.solve(input))
    
    "part a" in:
      linesFor(Day.`8`, Input.real, Part.a).use: input =>
        timed:
          IO.println(PartA.solve(input))

    "sample part b" in:
      linesFor(Day.`8`, Input.sample, Part.b).use: input =>
        IO.println(PartB.solve(input))

    "part b" in:
      linesFor(Day.`8`, Input.real, Part.b).use: input =>
        timed:
          IO.println(PartB.solve(input))
  }

  "Day 9" - {
    sealed trait Direction
    case object Up extends Direction
    case object Down extends Direction
    case object Right extends Direction
    case object Left extends Direction

    case class Position(x: Int, y: Int)

    def parseInput(movements: List[String]): List[(Direction, Int)] =
      movements.map: m =>
        val Array(direction, size) = m.split(" ")
        direction match
          case "R" => (Right, size.toInt)
          case "L" => (Left, size.toInt)
          case "U" => (Up, size.toInt)
          case "D" => (Down, size.toInt)
          case _   => fail()


    def moveOneStep(pos: Position, direction: Direction): Position =
      direction match
        case Up    => pos.copy(y = pos.y + 1)
        case Down  => pos.copy(y = pos.y - 1)
        case Left  => pos.copy(x = pos.x - 1)
        case Right => pos.copy(x = pos.x + 1)
    
    def catchTailUp(head: Position, tail: Position): Position =
      val (dx, dy) = ((head.x - tail.x), (head.y - tail.y))
      tail.copy(x = tail.x + dx.sign * 1, y = tail.y + dy.sign * 1)

    def areTouching(head: Position, tail: Position): Boolean =
      (Math.abs(head.x - tail.x) <= 1 && Math.abs(head.y - tail.y) <= 1)

    object PartA:
      def solve(movements: List[(Direction, Int)]): Int =
        val initialPosition = Position(0, 0)
        val (_, _, seen) = movements.foldLeft((initialPosition, initialPosition, Set(initialPosition))) { case ((head, tail, seen), movement) =>
          val (direction, size) = movement
          (1 to size).foldLeft((head, tail, seen)) { case ((head, tail, seen), _) =>
            val newHead = moveOneStep(head, direction)
            if (areTouching(newHead, tail))
              (newHead, tail, seen)
            else
              (newHead, head, seen + head)
          }
        }
        seen.size

    object PartB:
      def go(head: Position, tail: List[Position]): List[Position] =
        @tailrec
        def catchUp(knots: List[Position], done: List[Position]): List[Position] =
          knots match
            case Nil          => done
            case head :: tail =>
              if (areTouching(done.head, head))
                catchUp(tail, head +: done)
              else
                val newHead = catchTailUp(done.head, head)
                catchUp(tail, newHead +: done)
                
        catchUp(tail, List(head)).dropRight(1)

      def solve(movements: List[(Direction, Int)]): Int =
        val headInitialPosition = Position(0, 0)
        val tailInitialPosition = (1 to 9).map: i =>
          headInitialPosition
        .toList
        val (_, _, seen) = movements.foldLeft((headInitialPosition, tailInitialPosition, Set(headInitialPosition))) { case ((head, tail, seen), movement) =>
          val (direction, size) = movement
          (1 to size).foldLeft((head, tail, seen)) { case ((head, tail, seen), _) =>
            val newHead = moveOneStep(head, direction)
            val newTail = go(newHead, tail.reverse)
            (newHead, newTail, seen + newTail.head)
          }
        }
        seen.size

    "sample part a" in:
      linesFor(Day.`9`, Input.sample, Part.a).use: input =>
        IO.println(PartA.solve(parseInput(input)))
    
    "part a" in:
      linesFor(Day.`9`, Input.real, Part.a).use: input =>
        timed: 
          IO.println(PartA.solve(parseInput(input)))

    "sample part b" in:
      linesFor(Day.`9`, Input.sample, Part.b).use: input =>
        IO.println(PartB.solve(parseInput(input)))
    
    "part b" in:
      linesFor(Day.`9`, Input.real, Part.b).use: input =>
        timed: 
          IO.println(PartB.solve(parseInput(input)))
  }
