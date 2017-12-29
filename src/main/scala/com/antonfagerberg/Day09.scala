package com.antonfagerberg

import scala.annotation.tailrec

object Day09 {
  @tailrec
  private def groupCount(input: String, depth: Int, count: Int, garbage: Boolean): Int = {
    input.headOption match {
      case None => count
      case Some('!') => groupCount(input.tail.tail, depth, count, garbage)
      case Some('>') => groupCount(input.tail, depth, count, garbage = false)
      case Some(_) if garbage => groupCount(input.tail, depth, count, garbage)
      case Some('<') => groupCount(input.tail, depth, count, garbage = true)
      case Some(',') => groupCount(input.tail, depth, count, garbage)
      case Some('}') => groupCount(input.tail, depth - 1, count, garbage)
      case Some('{') => groupCount(input.tail, depth + 1, count + depth, garbage)
    }
  }

  @tailrec
  private def garbageCount(input: String, count: Int, garbage: Boolean): Int = {
    input.headOption match {
      case None => count
      case Some('!') => garbageCount(input.tail.tail, count, garbage)
      case Some('>') => garbageCount(input.tail, count, garbage = false)
      case Some(_) if garbage => garbageCount(input.tail, count + 1, garbage)
      case Some('<') => garbageCount(input.tail, count, garbage = true)
      case Some(',') => garbageCount(input.tail, count, garbage)
      case Some('}') => garbageCount(input.tail, count, garbage)
      case Some('{') => garbageCount(input.tail, count, garbage)
    }
  }

  val part1: String => Int = groupCount(_, 1, 0, garbage = false)
  val part2: String => Int = garbageCount(_, 0, garbage = false)
}

object Day09Solution extends App {
  private lazy val input = Input.getLines("day09/input").mkString

  println("Part 1: " + Day09.part1(input))
  println("Part 2: " + Day09.part2(input))
}