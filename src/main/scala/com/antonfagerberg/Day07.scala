package com.antonfagerberg

object Day07 {
  type Node = (String, Int, List[String])

  def parse(input: List[String]): List[Node] = {
    val parts = raw"(\w+) \((\d+)\)[ \->]*([a-z, ]*)".r

    input.map { case parts(name, weight, connections) =>
      (name, weight.toInt, connections.split(',').map(_.trim).toList.filter(_.nonEmpty))
    }
  }

  def findRoot(nodes: List[Node]): Option[String] = {
    nodes
      .find { case (name, _, connections) =>
        connections.nonEmpty && !nodes.exists { case (_, _, otherConnections) => otherConnections.contains(name) }
      }
      .map { case (name, _, _) => name }
  }

  def findUnbalanced(nodes: List[Node]): Option[Int] = {
    val nodeMap =
      nodes
        .map { case (name, weight, connections) => name -> (weight, connections) }
        .toMap

    def findUnbalanced(currentNode: String): Either[Int, Int] = {
      val (weight, connections) = nodeMap(currentNode)


      if (connections.isEmpty) {
        Left(weight)
      } else {
        val childBalance = connections.map(findUnbalanced)

        childBalance
          .find(_.isRight)
          .getOrElse {
            val balanceGroup =
              connections
                .zip(childBalance)
                .groupBy { case (_, balance) => balance.left.get }
                .map { case (combinedWeight, group) => (combinedWeight, group.map { case (name, _) => name }) }
                .toList

            balanceGroup match {
              case List((distinctWeight, group)) =>
                Left(weight + distinctWeight * group.size)

              case List((incorrectWeight, List(nodeName)), (correctWeight, _)) =>
                val diff = correctWeight - incorrectWeight
                val (nodeWeight, _) = nodeMap(nodeName)
                Right(nodeWeight + diff)

              case List((correctWeight, _), (incorrectWeight, List(nodeName))) =>
                val diff = correctWeight - incorrectWeight
                val (nodeWeight, _) = nodeMap(nodeName)
                Right(nodeWeight + diff)
            }
          }
      }
    }

    findRoot(nodes)
      .map(findUnbalanced)
      .flatMap(_.right.toOption)
  }

  val part1: List[String] => Option[String] = (Day07.parse _).andThen(Day07.findRoot)
  val part2: List[String] => Option[Int] = (Day07.parse _).andThen(Day07.findUnbalanced)
}

object Day07Solution extends App {
  private lazy val input = Input.getLines("day07/input")

  println("Part 1: " + Day07.part1(input))
  println("Part 2: " + Day07.part2(input))
}