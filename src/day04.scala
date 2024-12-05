package day04

import locations.Directory.currentDir
import inputs.Input.loadFileSync
import scala.util.{Try, Success}

@main def part1: Unit =
  println(s"The solution is ${part1(loadInput())}")

@main def part2: Unit =
  println(s"The solution is ${part2(loadInput())}")

def loadInput(): String = loadFileSync(s"$currentDir/../input/day04")

def parse(input: String): List[List[Char]] =
  input.split("\n").map(_.toList).toList

case class Delta(dx: Int, dy: Int)

def part1(input: String): String =
  val grid = parse(loadInput())

  val dirs = List(
    Delta(0, 1),
    Delta(1, 0),
    Delta(0, -1),
    Delta(-1, 0),
    Delta(1, 1),
    Delta(-1, 1),
    Delta(1, -1),
    Delta(-1, -1)
  )

  def extend(x: Int, y: Int, delta: Delta): Try[String] =
    Try {
      (0 to 3).map { n =>
        grid(y + delta.dy * n)(x + delta.dx * n)
      }.mkString
    }

  def resolve(value: Try[String]): Int =
    value match
      case Success("XMAS") => 1
      case _               => 0

  val validCount = (for {
    x <- 0 until grid(0).length
    y <- 0 until grid.length
    validDirections = dirs.map(delta => resolve(extend(x, y, delta)))
  } yield validDirections.sum).sum

  validCount.toString
end part1

def part2(input: String): String =
  val grid = parse(loadInput())

  val dirs = List(
    Delta(1, 1),
    Delta(1, -1)
  )

  def extend(x: Int, y: Int, delta: Delta): Try[String] =
    Try {
      (0 to 2).map { n =>
        grid(y + delta.dy * n)(x + delta.dx * n)
      }.mkString
    }

  def resolve(value: Try[String]): Boolean =
    value match
      case Success("MAS") => true
      case Success("SAM") => true
      case _              => false

  val validCount = (for {
    x <- 0 until grid(0).length
    y <- 0 until grid.length
    if resolve(extend(x, y, dirs(0))) && resolve(extend(x, y + 2, dirs(1)))
  } yield 1).sum

  validCount.toString
end part2
