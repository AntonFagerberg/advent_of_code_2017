package com.antonfagerberg

import org.scalatest.{FlatSpec, Matchers}

class Day06Test extends FlatSpec with Matchers {
  "Day 06 part 1" should "redistribute correct number of times" in {
    Day06.part1(List(0, 2, 7, 0)) shouldBe 5
  }

  "Day 06 part 2" should "find the correct redistribute loop count" in {
    Day06.part2(List(0, 2, 7, 0)) shouldBe 4
  }
}
