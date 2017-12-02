package com.antonfagerberg

object Day01 extends App {
  def captcha(offset: Int)(input: String): Int = {
    input
      .zip(input.drop(offset) + input)
      .filter { case (x, y) => x == y }
      .map { case (x, _) => x.asDigit }
      .sum
  }

  val part1: String => Int = captcha(1)
  val part2: String => Int = captcha(input.length / 2)

  private lazy val input = Input.getLines("day01/input").mkString

  println("Part 1: " + part1(input))
  println("Part 2: " + part2(input))
}
