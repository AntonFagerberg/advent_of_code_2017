package com.antonfagerberg

object Day04 {
  def part1(passPhrases: List[String]): Int = {
    passPhrases
      .map(_.split(' '))
      .count(words => words.length == words.distinct.length)
  }

  def part2(passPhrases: List[String]): Int = {
    passPhrases
      .map(_.split(' ').map(_.sorted))
      .count(words => words.length == words.distinct.length)
  }
}

object Day04Solution extends App {
  private lazy val input = Input.getLines("day04/input")

  println("Part 1: " + Day04.part1(input))
  println("Part 2: " + Day04.part2(input))
}
