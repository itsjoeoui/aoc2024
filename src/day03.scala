package day03

import locations.Directory.currentDir
import inputs.Input.loadFileSync
import scala.annotation.tailrec

@main def part1: Unit =
  println(s"The solution is ${part1(loadInput())}")

@main def part2: Unit =
  println(s"The solution is ${part2(loadInput())}")

def loadInput(): String = loadFileSync(s"$currentDir/../input/day03")

def part1(input: String): String =
  val pattern = """mul\((\d+),(\d+)\)""".r
  pattern
    .findAllIn(input)
    .toList
    .map { case pattern(a, b) => a.toInt * b.toInt }
    .sum
    .toString
end part1

def part2(input: String): String =

  @tailrec
  def parseRecursive(line: String, acc: Int = 0, include: Boolean = true): Int =
    line.match {
      case ""              => acc
      case s"do()$tail"    => parseRecursive(tail, acc, true)
      case s"don't()$tail" => parseRecursive(tail, acc, false)
      case s"mul(${a},${b})$tail"
          if include && a.forall(_.isDigit) && b.forall(_.isDigit) =>
        parseRecursive(tail, acc + a.toInt * b.toInt, include)
      case s"mul(${a},${b})$tail" if !include =>
        parseRecursive(tail, acc, include)
      case _ => parseRecursive(line.tail, acc, include)
    }

  parseRecursive(input).toString()
end part2
