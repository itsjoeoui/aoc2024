package day02

import locations.Directory.currentDir
import inputs.Input.loadFileSync

@main def part1: Unit =
  println(s"The solution is ${part1(loadInput())}")

@main def part2: Unit =
  println(s"The solution is ${part2(loadInput())}")

def loadInput(): String = loadFileSync(s"$currentDir/../input/day02")

def isReportSafe(report: Seq[Int]): Boolean = {
  val isIncreasing = report.sliding(2).forall { case Seq(a, b) =>
    b - a >= 1 && b - a <= 3
  }

  val isDecreasing = report.sliding(2).forall { case Seq(a, b) =>
    a - b >= 1 && a - b <= 3
  }

  isIncreasing || isDecreasing
}

def parseLine(line: String): List[Int] =
  line.trim
    .split("\\s+")
    .map(_.toInt)
    .toList

def part1(input: String): String =
  input.trim
    .split("\n")
    .foldLeft(0) { (acc, line) =>
      val report = parseLine(line)

      if (isReportSafe(report)) acc + 1 else acc
    }
    .toString
end part1

def part2(input: String): String =
  input.trim
    .split("\n")
    .foldLeft(0) { (acc, line) =>
      val report = parseLine(line)

      if (report.indices.map(report.patch(_, Nil, 1)).exists(isReportSafe))
        acc + 1
      else acc
    }
    .toString
end part2
