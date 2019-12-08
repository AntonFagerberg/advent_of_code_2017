package com.antonfagerberg

object Day24 {

  def findStrongest(target: Int, availablePorts: List[(Int, Int)]): Int = {
    availablePorts.map {
      case port@(`target`, other) => target + other + findStrongest(other, availablePorts.filterNot(_ == port))
      case port@(other, `target`) => target + other + findStrongest(other, availablePorts.filterNot(_ == port))
      case _ => 0
    }.max
  }

  def findLengths(port: (Int, Int), availablePorts: List[(Int, Int)]): List[List[(Int, Int)]] = {
    val (_, connection) = port

    val result = availablePorts.flatMap {
      case newPort@(`connection`, _) =>
        findLengths(newPort, availablePorts.filterNot(_ == newPort)).map(port :: _)

      case newPort@(target, `connection`) =>
        findLengths((connection, target), availablePorts.filterNot(_ == newPort)).map(port :: _)

      case _ =>
        List(List(port))
    }

    val maxSize = result.maxBy(_.size).size

    result.filter(_.size == maxSize)
  }

  val parse: Iterable[String] => List[(Int, Int)] =
    _.map(line => line.split('/')).map(parts => parts(0).toInt -> parts(1).toInt).toList

  def part1(input: Iterable[String]): Int = {
    findStrongest(0, parse(input))
  }

  def part2(input: Iterable[String]): Int = {
    val (_, sum) = findLengths((0, 0), parse(input))
      .foldLeft((0, 0)) { case ((maxSize, maxSum), bridge) =>
        lazy val sum = bridge.map { case (i, o) => i + o }.sum

        if (bridge.size >= maxSize && sum > maxSum) {
          (bridge.size, sum)
        } else {
          (maxSize, maxSum)
        }
      }

    sum
  }

}

object Day24Solution extends App {
  private lazy val input = Input.getLines("day24/input")
  println("Part 1: " + Day24.part1(input))
  println("Part 2: " + Day24.part2(input))
}