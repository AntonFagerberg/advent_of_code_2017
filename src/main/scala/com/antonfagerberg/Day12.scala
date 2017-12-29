package com.antonfagerberg

import scala.annotation.tailrec

object Day12 {
  def parse(input: List[String]): Set[Set[Int]] = {
    input
      .map { line =>
        line
          .split("[ <\\->,]+")
          .map(_.toInt)
          .toSet
      }
      .toSet
  }

  @tailrec
  private def connectionCount(traversedNodes: Set[Int])(connections: Set[Set[Int]]): Int = {
    val newTraversedNodes = connections.filter(_.exists(traversedNodes.contains)).reduce(_ ++ _)

    if (traversedNodes.size == newTraversedNodes.size) {
      traversedNodes.size
    } else {
      connectionCount(newTraversedNodes)(connections)
    }
  }

  private def clusterCount(connections: Set[Set[Int]]): Int = {
    val mergedConnections =
      connections.foldLeft(Set(Set.empty[Int])) { case (acc, nodes) =>
        val (connected, disconnected) = acc.partition(_.exists(nodes.contains))
        (disconnected + connected.foldLeft(nodes)(_ ++ _)).filter(_.nonEmpty)
      }

    if (mergedConnections == connections) {
      mergedConnections.size
    } else {
      clusterCount(mergedConnections)
    }
  }

  val part1: List[String] => Int = (parse _).andThen(connectionCount(Set(0))(_))
  val part2: List[String] => Int = (parse _).andThen(clusterCount)
}

object Day12Solution extends App {
  private lazy val input = Input.getLines("day12/input")

  println("Part 1: " + Day12.part1(input))
  println("Part 2: " + Day12.part2(input))
}