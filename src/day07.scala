package day07

import locations.Directory.currentDir
import inputs.Input.loadFileSync
import scala.util.Try

@main def part1: Unit =
  println(s"The solution is ${part1(loadInput())}")

@main def part2: Unit =
  println(s"The solution is ${part2(loadInput())}")

def loadInput(): String = loadFileSync(s"$currentDir/../input/day07")

case class Problem(target: Long, numbers: List[Long])

def parseInput(input: String): List[Problem] =
  input.linesIterator.flatMap { line =>
    line.split(": ").toList match {
      case List(target, nums) =>
        Try {
          Problem(
            target.toLong,
            nums.split(" ").map(_.toLong).toList
          )
        }.toOption
      case _ => None
    }
  }.toList

enum Operation:
  case Add, Multiply, Concat

def isValid(
    operations: List[Operation],
    target: Long,
    nums: List[Long],
    idx: Int,
    temp: Long
): Boolean =
  if (temp > target && nums.drop(idx).forall(_ > 0)) return false

  if (idx == nums.length) return temp == target

  return operations.exists {
    case Operation.Add =>
      isValid(operations, target, nums, idx + 1, temp + nums(idx))
    case Operation.Multiply =>
      isValid(operations, target, nums, idx + 1, temp * nums(idx))
    case Operation.Concat =>
      isValid(operations, target, nums, idx + 1, s"${temp}${nums(idx)}".toLong)
  }
end isValid

def part1(input: String): String =
  val problems = parseInput(input)

  val operations = List(Operation.Add, Operation.Multiply)

  problems
    .foldLeft(0L) { (acc, problem) =>
      acc + (if (
               problem.numbers match {
                 case first :: rest =>
                   isValid(
                     operations,
                     problem.target,
                     problem.numbers,
                     1,
                     first
                   )
                 case _ => false
               }
             ) problem.target
             else 0)
    }
    .toString
end part1

def part2(input: String): String =
  val problems = parseInput(input)

  val operations = List(Operation.Add, Operation.Multiply, Operation.Concat)

  problems
    .foldLeft(0L) { (acc, problem) =>
      acc + (if (
               problem.numbers match {
                 case first :: rest =>
                   isValid(
                     operations,
                     problem.target,
                     problem.numbers,
                     1,
                     first
                   )
                 case _ => false
               }
             ) problem.target
             else 0)
    }
    .toString
end part2
