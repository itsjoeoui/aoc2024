package day01

import locations.Directory.currentDir
import inputs.Input.loadFileSync
import scala.collection.mutable.PriorityQueue

@main def part1: Unit =
  println(s"The solution is ${part1(loadInput())}")

@main def part2: Unit =
  println(s"The solution is ${part2(loadInput())}")

def loadInput(): String = loadFileSync(s"$currentDir/../input/day01")

def part1(input: String): String =
  val list1 = PriorityQueue.empty[Int]
  val list2 = PriorityQueue.empty[Int]

  def insertNumber(line: String): Unit =
    val firstDigit = line.find(_.isDigit).get.asDigit
    val lastDigit = line.findLast(_.isDigit).get.asDigit
    list1.enqueue(firstDigit)
    list2.enqueue(lastDigit)
    ()

  input.linesIterator.foreach(insertNumber)

  var total = 0

  while (list1.nonEmpty && list2.nonEmpty) do
    val first = list1.dequeue()
    val last = list2.dequeue()

    total += math.abs(first - last)

  total.toString()

end part1

def part2(input: String): String =
  ""
end part2
