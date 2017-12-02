package com.antonfagerberg

object Day01 extends App {
  def captcha(offset: Int)(input: String): Int = {
    input
      .zip(input.drop(offset) + input)
      .filter { case (x, y) => x == y }
      .map { case (x, _) => x.asDigit }
      .sum
  }

  def part1(input: String): Int = captcha(1)(input)

  def part2(input: String): Int = captcha(input.length / 2)(input)

  private lazy val input = Input.getLines("day01/input").mkString

  println("Part 1:" + part1(input))
  println("Part 2:" + part2(input))
}
