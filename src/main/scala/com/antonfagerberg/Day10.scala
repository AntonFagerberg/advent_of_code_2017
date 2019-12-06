package com.antonfagerberg

import scala.annotation.tailrec

object Day10 {

  @tailrec
  def hash(input: List[Int], lengths: List[Int], position: Int = 0, skipSize: Int = 0): (List[Int], Int, Int) = {
    if (lengths.isEmpty) {
      (input, position, skipSize)
    } else {
      val length :: lengthsRest = lengths

      val nextPosition = (position + length + skipSize) % input.size

      if (position + length < input.size) {
        val (start, temp) = input.splitAt(position)
        val (mid, end) = temp.splitAt(length)

        hash(start ++ mid.reverse ++ end, lengthsRest, nextPosition, skipSize + 1)
      } else {
        val (temp, end) = input.splitAt(position)
        val (start, mid) = temp.splitAt(position + length - input.size)
        val reversed = (end ++ start).reverse

        hash(reversed.takeRight(start.size) ++ mid ++ reversed.take(end.size), lengthsRest, nextPosition, skipSize + 1)
      }
    }
  }

  def part1(lengths: List[Int]): Int = {
    val (values, _, _) = hash((0 to 255).toList, lengths)
    val a :: b :: _ = values
    a * b
  }

  def part2(lengths: String): String = {
    val asciiLengths = lengths.map(_.toInt).toList ++ List(17, 31, 73, 47, 23)

    val (result, _, _) = (0 until 63).foldLeft(hash((0 to 255).toList, asciiLengths)) {
      case ((input, position, skipSize), _) => hash(input, asciiLengths, position, skipSize)
    }

    result
      .sliding(16, 16)
      .map(_.reduce(_ ^ _).toHexString)
      .map("0" + _)
      .map(_.takeRight(2)).mkString
  }

}

object Day10Solution extends App {
  private lazy val input = Input.getLines("day10/input").head

  println("Part 1: " + Day10.part1(input.split(",").map(_.toInt).toList))
  println("Part 2: " + Day10.part2(input))
}
