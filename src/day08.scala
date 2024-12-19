package day08

import locations.Directory.currentDir
import inputs.Input.loadFileSync

@main def part1: Unit =
  println(s"The solution is ${part1(loadInput())}")

@main def part2: Unit =
  println(s"The solution is ${part2(loadInput())}")

def loadInput(): String = loadFileSync(s"$currentDir/../input/day08")

case class Coord(x: Int, y: Int)

case class Problem(grid: Seq[Seq[Char]], antennas: Map[Char, Seq[Coord]])

def parseInput(input: String): Problem =
  val grid = input.split("\n").map(_.toList).toList

  val antennas = for {
    y <- grid.indices
    x <- grid(y).indices
    char = grid(y)(x)
    if char != '.'
  } yield (char, Coord(x, y))

  Problem(
    grid,
    antennas.groupMap(_._1)(_._2).toMap()
  )

def getAntinodePair(a: Coord, b: Coord): (Coord, Coord) =
  val dx = a.x - b.x
  val dy = a.y - b.y

  (Coord(a.x + dx, a.y + dy), Coord(b.x - dx, b.y - dy))

def getAntinodes(a: Coord, b: Coord, x: Int, y: Int): Seq[Coord] =
  val dx = a.x - b.x
  val dy = a.y - b.y

  val antinodes = for {
    i <- 0 until x
    j <- 0 until y
    if (i - a.x) * dy == (j - a.y) * dx // Check if the point (i, j) is in line with a and b
  } yield Coord(i, j)

  antinodes

def isValidCoord(coord: Coord, problem: Problem): Boolean =
  coord.x >= 0 &&
    coord.x < problem.grid(0).length &&
    coord.y >= 0 &&
    coord.y < problem.grid.length

def part1(input: String): String =
  val problem = parseInput(input)

  problem.antennas
    .flatMap { case (char, coords) =>
      coords
        .combinations(2)
        .flatMap { case Seq(a, b) =>
          getAntinodePair(a, b).toList
        }
    }
    .toSet
    .count(coord => isValidCoord(coord, problem))
    .toString

end part1

def part2(input: String): String =
  val problem = parseInput(input)

  problem.antennas
    .flatMap { case (char, coords) =>
      coords
        .combinations(2)
        .flatMap { case Seq(a, b) =>
          getAntinodes(a, b, problem.grid(0).length, problem.grid.length)

        }
    }
    .toSet
    .count(coord => isValidCoord(coord, problem))
    .toString
end part2
