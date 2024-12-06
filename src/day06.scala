package day06

import locations.Directory.currentDir
import inputs.Input.loadFileSync
import scala.annotation.tailrec

@main def part1: Unit =
  println(s"The solution is ${part1(loadInput())}")

@main def part2: Unit =
  println(s"The solution is ${part2(loadInput())}")

def loadInput(): String = loadFileSync(s"$currentDir/../input/day06")

def parseInput(input: String): List[List[Char]] =
  input.split("\n").map(_.toList).toList

case class Coord(x: Int, y: Int)

enum Direction:
  case Up, Right, Down, Left

val directions =
  List(Direction.Up, Direction.Right, Direction.Down, Direction.Left)

def rotateRight(direction: Direction): Direction =
  directions((directions.indexOf(direction) + 1) % directions.length)

def nextPosition(coord: Coord, direction: Direction): Coord =
  direction match
    case Direction.Up    => Coord(coord.x, coord.y - 1)
    case Direction.Down  => Coord(coord.x, coord.y + 1)
    case Direction.Left  => Coord(coord.x - 1, coord.y)
    case Direction.Right => Coord(coord.x + 1, coord.y)

def part1(input: String): String =
  val grid = parseInput(input)

  def outOfBound(coord: Coord): Boolean =
    coord.x < 0 || coord.x >= grid(
      0
    ).length || coord.y < 0 || coord.y >= grid.length

  @tailrec
  def traverse(
      coord: Coord,
      visited: Set[Coord],
      direction: Direction
  ): Set[Coord] =
    val newVisited = visited + coord

    val nextCoord = nextPosition(coord, direction)

    if (outOfBound(nextCoord)) {
      newVisited
    } else if (grid(nextCoord.y)(nextCoord.x) == "#".head) {
      val newDirection = rotateRight(direction)
      traverse(coord, newVisited, newDirection)
    } else {
      traverse(nextCoord, newVisited, direction)
    }

  val start = grid.zipWithIndex.collectFirst {
    case (row, rowIndex) if row.contains('^') =>
      Coord(row.indexWhere(_ == "^".head), rowIndex)
  }

  val result = start match
    case Some(coord) =>
      traverse(coord, Set(), Direction.Up)
    case None => Set()

  result.size.toString()
end part1

def part2(input: String): String =
  val grid = parseInput(input)

  def outOfBound(coord: Coord): Boolean =
    coord.x < 0 || coord.x >= grid(
      0
    ).length || coord.y < 0 || coord.y >= grid.length

  @tailrec
  def traverse(
      coord: Coord,
      visited: Set[Coord],
      direction: Direction
  ): Set[Coord] =
    val newVisited = visited + coord

    val nextCoord = nextPosition(coord, direction)

    if (outOfBound(nextCoord)) {
      newVisited
    } else if (grid(nextCoord.y)(nextCoord.x) == "#".head) {
      val newDirection = rotateRight(direction)
      traverse(coord, newVisited, newDirection)
    } else {
      traverse(nextCoord, newVisited, direction)
    }

  @tailrec
  def cycleDetect(
      coord: Coord,
      visited: Set[String],
      direction: Direction,
      extra: Coord
  ): Boolean =
    val nextCoord = nextPosition(coord, direction)

    if (visited.contains(s"${coord.toString()}${direction.toString()}")) {
      true
    } else if (outOfBound(nextCoord)) {
      false
    } else if (
      grid(nextCoord.y)(
        nextCoord.x
      ) == "#".head || (nextCoord.x == extra.x && nextCoord.y == extra.y)
    ) {
      val newDirection = rotateRight(direction)
      cycleDetect(coord, visited, newDirection, extra)
    } else {
      cycleDetect(
        nextCoord,
        visited + s"${coord.toString()}${direction.toString}",
        direction,
        extra
      )
    }

  val startOption = grid.zipWithIndex.collectFirst {
    case (row, rowIndex) if row.contains('^') =>
      Coord(row.indexWhere(_ == "^".head), rowIndex)
  }

  val start = startOption match
    case Some(coord) => coord
    case _           => throw new Exception("No starting point found")

  val possibilities = traverse(start, Set(), Direction.Up) - start

  possibilities
    .count { coord =>
      cycleDetect(
        start,
        Set(),
        Direction.Up,
        coord
      )
    }
    .toString()
end part2
