package com.antonfagerberg

import org.scalatest.{FlatSpec, Matchers}

class Day14Test extends FlatSpec with Matchers {
  "Day 14" should "convert to binary" in {
    Day14.toBinary("a0c2017").mkString shouldBe "1010000011000010000000010111"
  }
}
