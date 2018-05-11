package com.antonfagerberg

import org.scalatest.{FlatSpec, Matchers}

class Day17Test extends FlatSpec with Matchers {
  "Day 17 part 1" should "build buffer" in {
    Day17.buildBuffer(1, 3).takeToList(2) shouldBe List(1, 0)
    Day17.buildBuffer(2, 3).takeToList(3) shouldBe List(2, 1, 0)
    Day17.buildBuffer(3, 3).takeToList(4) shouldBe List(3, 1, 0, 2)
  }

  "Day 17 part 1" should "find value after 2017" in {
    Day17.buildBuffer(2017, 3).next.value shouldBe 638
  }

}
