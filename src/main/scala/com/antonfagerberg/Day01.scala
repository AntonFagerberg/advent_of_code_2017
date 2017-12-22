package com.antonfagerberg

object Day01 {
  def captcha(offset: Int)(input: String): Int = {
    input
      .zip(input.drop(offset) + input)
      .filter { case (x, y) => x == y }
      .map { case (x, _) => x.asDigit }
      .sum
  }

  val part1: String => Int = captcha(1)
  val part2: String => Int = input => captcha(input.length / 2)(input)
}

object Day01Solution extends App {
  private lazy val input = Input.getLines("day01/input").mkString

  println("Part 1: " + Day01.part1(input))
  println("Part 2: " + Day01.part2(input))

}