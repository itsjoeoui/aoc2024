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

enum Direction:
  case Up, Right, Down, Left

val directions =
  List(Direction.Up, Direction.Right, Direction.Down, Direction.Left)

class RobotPathTracker(grid: List[List[Char]]) {
  case class Coord(x: Int, y: Int)

  def outOfBounds(coord: Coord): Boolean =
    coord.x < 0 || coord.x >= grid(
      0
    ).length || coord.y < 0 || coord.y >= grid.length

  def nextPosition(coord: Coord, direction: Direction): Coord =
    direction match {
      case Direction.Up    => Coord(coord.x, coord.y - 1)
      case Direction.Right => Coord(coord.x + 1, coord.y)
      case Direction.Down  => Coord(coord.x, coord.y + 1)
      case Direction.Left  => Coord(coord.x - 1, coord.y)
    }

  def rotateRight(direction: Direction): Direction =
    directions((directions.indexOf(direction) + 1) % directions.length)

  def findStart: Option[Coord] =
    grid.zipWithIndex.collectFirst {
      case (row, rowIndex) if row.contains('^') =>
        Coord(row.indexWhere(_ == '^'), rowIndex)
    }

  @tailrec
  final def traverse(
      coord: Coord,
      visited: Set[Coord],
      direction: Direction
  ): Set[Coord] = {
    val newVisited = visited + coord
    val nextCoord = nextPosition(coord, direction)

    if (outOfBounds(nextCoord)) {
      newVisited
    } else if (grid(nextCoord.y)(nextCoord.x) == '#') {
      traverse(coord, newVisited, rotateRight(direction))
    } else {
      traverse(nextCoord, newVisited, direction)
    }
  }

  @tailrec
  final def cycleDetect(
      coord: Coord,
      visited: Set[String],
      direction: Direction,
      extra: Coord
  ): Boolean = {
    val stateKey = s"${coord.toString}${direction.toString}"
    val nextCoord = nextPosition(coord, direction)

    if (visited.contains(stateKey)) {
      true
    } else if (outOfBounds(nextCoord)) {
      false
    } else if (grid(nextCoord.y)(nextCoord.x) == '#' || nextCoord == extra) {
      cycleDetect(coord, visited, rotateRight(direction), extra)
    } else {
      cycleDetect(nextCoord, visited + stateKey, direction, extra)
    }
  }
}

def part1(input: String): String = {
  val tracker = new RobotPathTracker(parseInput(input))
  (tracker.findStart match {
    case Some(start) =>
      tracker.traverse(start, Set(), Direction.Up).size
    case None => 0
  }).toString
}

def part2(input: String): String = {
  val tracker = new RobotPathTracker(parseInput(input))
  tracker.findStart match {
    case Some(start) =>
      val possibilities = tracker.traverse(start, Set(), Direction.Up) - start
      possibilities
        .count(coord => tracker.cycleDetect(start, Set(), Direction.Up, coord))
        .toString
    case None =>
      throw new Exception("No starting point found")
  }
}
