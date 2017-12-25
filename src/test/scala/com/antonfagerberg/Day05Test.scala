package com.antonfagerberg

import org.scalatest.{FlatSpec, Matchers}

class Day05Test extends FlatSpec with Matchers {
  "Day 05 part 1" should "jump correct number of jumps" in {
    Day05.part1(List(0, 3, 0, 1, -3)) shouldBe 5
  }

  "Day 05 part 2" should "jump correct number of jumps" in {
    Day05.part2(List(0, 3, 0, 1, -3)) shouldBe 10
  }
}
