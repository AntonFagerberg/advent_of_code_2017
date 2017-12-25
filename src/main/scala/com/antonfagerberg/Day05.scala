package com.antonfagerberg

import scala.annotation.tailrec
import scala.collection.mutable.ArrayBuffer

object Day05 {
  @tailrec
  private def jump(instructions: ArrayBuffer[Int] = ArrayBuffer.empty, target: Int = 0, jumpCount: Int = 0): Int = {
    instructions.lift(target) match {
      case Some(nextTarget) =>
        instructions(target) = 1 + nextTarget
        jump(instructions, target + nextTarget, jumpCount + 1)

      case None => jumpCount
    }
  }

  @tailrec
  private def jump2(instructions: ArrayBuffer[Int] = ArrayBuffer.empty, target: Int = 0, jumpCount: Int = 0): Int = {
    instructions.lift(target) match {
      case Some(nextTarget) =>
        if (nextTarget >= 3) {
          instructions(target) = -1 + nextTarget
        } else {
          instructions(target) = 1 + nextTarget
        }
        jump2(instructions, target + nextTarget, jumpCount + 1)

      case None => jumpCount
    }
  }

  val part1: Seq[Int] => Int = input => jump(input.to[ArrayBuffer])
  val part2: Seq[Int] => Int = input => jump2(input.to[ArrayBuffer])
}

object Day05Solution extends App {
  private lazy val input = Input.getLines("day05/input").map(_.toInt)

  println("Part 1: " + Day05.part1(input))
  println("Part 2: " + Day05.part2(input))
}
