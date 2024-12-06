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

    if outOfBound(nextCoord)
    then newVisited
    else if grid(nextCoord.y)(nextCoord.x) == "#".head then
      val newDirection = rotateRight(direction)
      traverse(coord, newVisited, newDirection)
    else traverse(nextCoord, newVisited, direction)

  val start = grid.zipWithIndex.collectFirst {
    case (row, rowIndex) if row.indexWhere(_ == "^".head) != -1 =>
      Coord(row.indexWhere(_ == "^".head), rowIndex)
  }

  val result = start match
    case Some(coord) =>
      traverse(coord, Set(), Direction.Up)
    case None => Set()

  result.size.toString()

end part1

def part2(input: String): String =
  ""
end part2
