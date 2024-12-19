package day01

import locations.Directory.currentDir
import inputs.Input.loadFileSync

@main def part1: Unit =
  println(s"The solution is ${part1(loadInput())}")

@main def part2: Unit =
  println(s"The solution is ${part2(loadInput())}")

def loadInput(): String = loadFileSync(s"$currentDir/../input/day01")

def part1(input: String): String =
  input.trim
    .split("\n")
    .map { line =>
      line.trim
        .split("\\s+")
        .map(_.toInt)
        .toList
    }
    .toList
    .transpose
    .map(_.sorted)
    .transpose
    .map { row => math.abs(row(0) - row(1)) }
    .sum
    .toString
end part1

def part2(input: String): String =
  val lists = input.trim
    .split("\n")
    .map { line =>
      line.trim
        .split("\\s+")
        .map(_.toInt)
        .toList
    }
    .toList
    .transpose

  val counts = lists(1).groupBy(identity).view.mapValues(_.size)

  lists(0).map { n => counts.getOrElse(n, 0) * n }.sum.toString
end part2
