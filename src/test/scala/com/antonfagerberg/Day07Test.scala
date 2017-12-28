package com.antonfagerberg

import org.scalatest.{FlatSpec, Matchers}

class Day07Test extends FlatSpec with Matchers {
  val testInput =
    """pbga (66)
      |xhth (57)
      |ebii (61)
      |havc (66)
      |ktlj (57)
      |fwft (72) -> ktlj, cntj, xhth
      |qoyq (66)
      |padx (45) -> pbga, havc, qoyq
      |tknk (41) -> ugml, padx, fwft
      |jptl (61)
      |ugml (68) -> gyxo, ebii, jptl
      |gyxo (61)
      |cntj (57)"""
      .stripMargin
      .split('\n')
      .toList

  "Day 07 part 1" should "parse input" in {
    val parsedInput = Day07.parse(testInput)

    val expectedResult =
      List(
        ("pbga", 66, Nil),
        ("xhth", 57, Nil),
        ("ebii", 61, Nil),
        ("havc", 66, Nil),
        ("ktlj", 57, Nil),
        ("fwft", 72, List("ktlj", "cntj", "xhth")),
        ("qoyq", 66, Nil),
        ("padx", 45, List("pbga", "havc", "qoyq")),
        ("tknk", 41, List("ugml", "padx", "fwft")),
        ("jptl", 61, Nil),
        ("ugml", 68, List("gyxo", "ebii", "jptl")),
        ("gyxo", 61, Nil),
        ("cntj", 57, Nil)
      )

    parsedInput shouldBe expectedResult
  }

  it should "find the root" in {
    Day07.part1(testInput) shouldBe Some("tknk")
  }

  "Day 07 part 2" should "find unbalanced program" in {
    Day07.part2(testInput) shouldBe Some(60)
  }
}
