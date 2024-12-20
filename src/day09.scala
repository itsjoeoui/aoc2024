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
  enum Block(val size: Int):
    case File(s: Int, id: Int) extends Block(s)
    case Empty(s: Int) extends Block(s)
    def getBlockId = this match
      case File(_, id) => Some(id)
      case Empty(_)    => None
    def canInsert(block: Block) = this match
      case Empty(size) => size >= block.size
      case _           => false

  extension (empty: Block.Empty)
    def insert(b: Block): Seq[Block] =
      if b.size < empty.size then Seq(b, Block.Empty(empty.size - b.size))
      else Seq(b)

  type System = Seq[Block]
  extension (system: System)
    def checksum: Long = system
      .flatMap(b => Seq.fill(b.size)(b.getBlockId.getOrElse(0)))
      .zipWithIndex
      .map(_.toLong * _)
      .sum

  val system = input
    .map(_.asDigit)
    .grouped(2)
    .zipWithIndex
    .flatMap {
      case (Seq(file, free), id) =>
        Seq(Block.File(file, id), Block.Empty(free))
      case (Seq(file), id) =>
        Seq(Block.File(file, id))
    }
    .toList

  def compact(system: System): System =
    @tailrec
    def rec(system: System, acc: System): System =
      system.lastOption match
        case None                             => acc
        case Some(last @ Block.Empty(_))      => rec(system.init, last +: acc)
        case Some(last @ Block.File(size, _)) =>
          // Find first block which can fit the file block
          val fitter = system.zipWithIndex
            .find((block, _) => block.canInsert(last))

          fitter match
            case None =>
              // If it doesn't fit anywhere, don't move it
              rec(system.init, last +: acc)
            case Some(free @ Block.Empty(_), id) =>
              // If it fits somewhere, insert inside this free block
              val newDisk =
                system.take(id) ++ free.insert(last) ++ system.drop(id + 1).init
              rec(newDisk, Block.Empty(last.size) +: acc)
            case _ => throw new MatchError("Unexpected block type")
    rec(system, Seq.empty)

  compact(system).checksum.toString
end part2
