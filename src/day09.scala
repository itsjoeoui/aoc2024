package day09

import locations.Directory.currentDir
import inputs.Input.loadFileSync
import scala.annotation.tailrec

@main def part1: Unit =
  println(s"The solution is ${part1(loadInput())}")

@main def part2: Unit =
  println(s"The solution is ${part2(loadInput())}")

def loadInput(): String = loadFileSync(s"$currentDir/../input/day09")

def part1(input: String): String =
  sealed trait Block
  case class FileBlock(fileId: Int) extends Block
  case object EmptyBlock extends Block

  def checksum(system: Seq[Block]): Long =
    system.zipWithIndex.foldLeft(0L) { case (acc, (block, index)) =>
      block match
        case FileBlock(fileId) =>
          acc + fileId * index
        case EmptyBlock =>
          acc
    }

  def compact(system: Seq[Block]): Seq[Block] =
    @tailrec
    def rec(system: Seq[Block], acc: Seq[Block]): Seq[Block] =
      if system.isEmpty then acc
      else
        system.head match
          case EmptyBlock   => rec(system.last +: system.tail.init, acc)
          case FileBlock(_) => rec(system.tail, acc :+ system.head)
    rec(system, Seq.empty)

  val system = input
    .map(_.asDigit)
    .grouped(2)
    .zipWithIndex
    .flatMap {
      case (Seq(file, free), id) =>
        List.fill(file)(FileBlock(id)) ++ List.fill(free)(EmptyBlock)
      case (Seq(file), id) => List.fill(file)(FileBlock(id))
    }
    .toList

  compact.andThen(checksum)(system).toString

end part1

def part2(input: String): String =
  ""
end part2
