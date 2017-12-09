package com.antonfagerberg

object Day03 {
  private def maxNumber(circle: Int): Int = {
    Math.pow(2 * circle + 1, 2).toInt
  }

  def circles(target: Int): Int = {
    Stream
      .from(0)
      .find(target <= maxNumber(_))
      .get
  }

  def steps(circle: Int, target: Int): Int = {
    if (circle == 0) {
      0
    } else {
      val maxSideSteps = circle * 2 + 1
      Math.abs((maxNumber(circle) - target) % (maxSideSteps - 1) - maxSideSteps / 2)
    }
  }

  lazy val part1: Int => Int = target => {
    val circleSteps = circles(target)
    circleSteps + steps(circleSteps, target)
  }

  println("Part 1: " + part1(347991))
}
