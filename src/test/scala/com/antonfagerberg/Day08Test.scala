package com.antonfagerberg

import org.scalatest.{FlatSpec, Matchers}

class Day08Test extends FlatSpec with Matchers {
  "Day 08 part 1" should "compute input" in {
    val registers =
      Day08.compute(
        """b inc 5 if a > 1
          |a inc 1 if b < 5
          |c dec -10 if a >= 1
          |c inc -20 if c == 10""".stripMargin.split('\n').toList
      )

    registers shouldBe Map("a" -> 1, "c" -> -10)
  }

  it should "find the largest register value" in {
    Day08.part1(
      """b inc 5 if a > 1
        |a inc 1 if b < 5
        |c dec -10 if a >= 1
        |c inc -20 if c == 10""".stripMargin.split('\n').toList
    ) shouldBe 1
  }

  "Day 08 part 2" should "max value during computation" in {
    val registers =
      Day08.part2(
        """b inc 5 if a > 1
          |a inc 1 if b < 5
          |c dec -10 if a >= 1
          |c inc -20 if c == 10""".stripMargin.split('\n').toList
      )

    registers shouldBe 10
  }
}
