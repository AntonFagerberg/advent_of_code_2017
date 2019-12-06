package com.antonfagerberg

import org.scalatest.{FlatSpec, Matchers}

class Day10Test extends FlatSpec with Matchers {
  "Day 10 part 1" should "knot hash" in {
    Day10.hash(List(0, 1, 2, 3, 4), List(3, 4, 1, 5)) shouldBe (List(3, 4, 2, 1, 0), 4, 4)
  }

  "Day 10 part 2" should "hash example values" in {
    Day10.part2("") shouldBe "a2582a3a0e66e6e86e3812dcb672a272"
    Day10.part2("AoC 2017") shouldBe "33efeb34ea91902bb2f59c9920caa6cd"
    Day10.part2("1,2,3") shouldBe "3efbe78a8d82f29979031a4aa0b16a9d"
    Day10.part2("1,2,4") shouldBe "63960835bcdc130f0b66d7ff4f6a5a8e"
  }
}
