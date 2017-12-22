package com.antonfagerberg

import com.antonfagerberg.Day02.{part1, part2}

object Day02 {
  def checksum1(input: List[List[Int]]): Int = {
    input.foldLeft(0) { (acc, row) =>
      acc + (row.max - row.min)
    }
  }

  def checksum2(input: List[List[Int]]): Int = {
    input.foldLeft(0) { (acc, row) =>
      acc +
        row
          .combinations(2)
          .map(_.sorted)
          .find { case List(x, y) => x != y && y % x == 0 }
          .map { case List(x, y) => y / x }
          .getOrElse(throw new RuntimeException("No evenly divisible numbers found"))
    }
  }

  def parse(input: List[String]): List[List[Int]] = {
    input
      .map(_.split("\\s+").map(_.toInt).toList)
      .filter(_.nonEmpty)
  }

  val part1: List[String] => Int = (Day02.parse _).andThen(Day02.checksum1)
  val part2: List[String] => Int = (Day02.parse _).andThen(Day02.checksum2)
}

object Day02Solution extends App {
  private lazy val input = Input.getLines("day02/input")

  println("Part 1: " + part1(input))
  println("Part 2: " + part2(input))
}
