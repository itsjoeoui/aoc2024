package day05

import locations.Directory.currentDir
import inputs.Input.loadFileSync

@main def part1: Unit =
  println(s"The solution is ${part1(loadInput())}")

@main def part2: Unit =
  println(s"The solution is ${part2(loadInput())}")

def loadInput(): String = loadFileSync(s"$currentDir/../input/day05")

def parse(input: String): List[List[Char]] =
  input.split("\n").map(_.toList).toList

class Puzzle(input: String) {
  private val (dependencies, updates): (Set[String], List[String]) = {
    val lines = input.split("\n").toList
    val separator = lines.indexOf("")
    (
      // This is a list of dependencies
      lines.take(separator).toSet,
      // This is a list of updates
      lines.drop(separator + 1)
    )
  }

  def getDependencies(): Set[String] = dependencies
  def getUpdates(): List[String] = updates
}

val puzzle = Puzzle(loadInput())

def part1(input: String): String =
  val dependencies = puzzle.getDependencies()
  val updates = puzzle.getUpdates()

  updates
    .map { update =>
      val entries = update.split(",").toList

      val valid = (for {
        y <- 0 until entries.length
        x <- 0 until y
        bad = dependencies.contains(s"${entries(y)}|${entries(x)}")
      } yield bad).forall(_ == false)

      if valid then entries(entries.length / 2).toInt else 0
    }
    .sum
    .toString()

end part1

def part2(input: String): String =
  val dependencies = puzzle.getDependencies()
  val updates = puzzle.getUpdates()

  updates
    .map { update =>
      val entries = update.split(",").toList

      val valid = (for {
        y <- 0 until entries.length
        x <- 0 until y
        bad = dependencies.contains(s"${entries(y)}|${entries(x)}")
      } yield bad).forall(_ == false)

      if valid then 0
      else
        entries
          .sortWith((a, b) => dependencies.contains(s"${a}|${b}"))(
            entries.length / 2
          )
          .toInt
    }
    .sum
    .toString()
end part2
