package com.antonfagerberg

import org.scalatest.{FlatSpec, Matchers}

class Day24Test extends FlatSpec with Matchers {
  "Day 24 part 1" should "find strongest example bridge" in {
    val input = """0/2
                  |2/2
                  |2/3
                  |3/4
                  |3/5
                  |0/1
                  |10/1
                  |9/10""".stripMargin.split("\n")

    Day24.part1(input) shouldBe 31
  }

  "Day 24 part 2" should "find longest & strongest example bridge" in {
    val input = """0/2
                  |2/2
                  |2/3
                  |3/4
                  |3/5
                  |0/1
                  |10/1
                  |9/10""".stripMargin.split("\n")

    Day24.part2(input) shouldBe 19
  }

}
