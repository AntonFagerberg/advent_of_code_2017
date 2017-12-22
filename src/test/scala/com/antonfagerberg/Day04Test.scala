package com.antonfagerberg

import org.scalatest.{FlatSpec, Matchers}

class Day04Test extends FlatSpec with Matchers {
  "Day 04 part 1" should "filter out duplicate words" in {
    Day04.part1(
      List(
        "aa bb cc dd ee",
        "aa bb cc dd aa",
        "aa bb cc dd aaa"
      )
    ) shouldBe 2
  }

  "Day 04 part 2" should "filter out duplicate anagram words" in {
    Day04.part2(
      List(
        "abcde fghij",
        "abcde xyz ecdab",
        "a ab abc abd abf abj",
        "iiii oiii ooii oooi oooo",
        "oiii ioii iioi iiio"
      )
    ) shouldBe 3
  }
}
