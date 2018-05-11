package com.antonfagerberg

import org.scalatest.{FlatSpec, Matchers}

class Day19Test extends FlatSpec with Matchers {

  private val input =
    Input
      .getLines("day19/test_input")
      .map(_.toCharArray)
      .toArray

  "Day 19 part 1" should "find start position" in {
    Day19.findStart(input) shouldBe (5, 0)
  }

  "Day 19 part 1 & 2" should "walk and collect letters & steps" in {
    val coordinate = Day19.findStart(input)

    Day19.walk(input, coordinate, (0, 1)) shouldBe ("ABCDEF", 38)
  }

}
