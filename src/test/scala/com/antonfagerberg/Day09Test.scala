package com.antonfagerberg

import org.scalatest.{FlatSpec, Matchers}

class Day09Test extends FlatSpec with Matchers {
  "Day 09 part 1" should "count groups" in {
    Day09.part1("{}") shouldBe 1
    Day09.part1("{{{}}}") shouldBe 6
    Day09.part1("{{},{}}") shouldBe 5
    Day09.part1("{{{},{},{{}}}}") shouldBe 16
    Day09.part1("{<a>,<a>,<a>,<a>}") shouldBe 1
    Day09.part1("{{<ab>},{<ab>},{<ab>},{<ab>}}") shouldBe 9
    Day09.part1("{{<!!>},{<!!>},{<!!>},{<!!>}}") shouldBe 9
    Day09.part1("{{<a!>},{<a!>},{<a!>},{<ab>}}") shouldBe 3
  }

  "Day 09 part 2" should "count garbage" in {
    Day09.part2("<>") shouldBe 0
    Day09.part2("<random characters>") shouldBe 17
    Day09.part2("<<<<>") shouldBe 3
    Day09.part2("<{!>}>") shouldBe 2
    Day09.part2("<!!>") shouldBe 0
    Day09.part2("<!!!>>") shouldBe 0
    Day09.part2("<{o\"i!a,<{i<a>,") shouldBe 10
  }
}
