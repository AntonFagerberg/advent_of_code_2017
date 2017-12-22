package com.antonfagerberg

import com.antonfagerberg.Day03.{part1, part2}

import scala.collection.immutable.HashMap

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

  private val transitions =
    List(
      (0, 1),
      (-1, 0),
      (0, -1),
      (1, 0)
    )

  def findNeighbours(state: HashMap[(Int, Int), Int], position: (Int, Int)): Int = {
    val (x, y) = position

    val numbers =
      for {
        xx <- (x - 1) to (x + 1)
        yy <- (y - 1) to (y + 1)
      } yield {
        state.getOrElse((xx, yy), 0)
      }

    numbers.sum
  }

  def spiral(target: Int,
             transitionStream: Stream[(Int, Int)] = Stream.continually(transitions).flatten,
             position: (Int, Int) = (1, 0),
             state: HashMap[(Int, Int), Int] = new HashMap() + ((0, 0) -> 1)): HashMap[(Int, Int), Int] = {
    val currentValue = findNeighbours(state, position)

    val newState = state + (position -> currentValue)

    if (currentValue > target) {
      newState
    } else {
      val (x, y) = position
      val (dx, dy) = transitionStream.tail.head

      if (state.contains((x + dx, y + dy))) {
        val (xx, yy) = transitionStream.head
        spiral(target, transitionStream, (x + xx, y + yy),  newState)
      } else {
        spiral(target, transitionStream.tail, (x + dx, y + dy),  newState)
      }
    }
  }

  val part1: Int => Int = target => {
    val circleSteps = circles(target)
    circleSteps + steps(circleSteps, target)
  }

  val part2: Int => Int = spiral(_).values.max
}

object Day03Solution extends App {
  println("Part 1: " + part1(347991))
  println("Part 2: " + part2(347991))
}
