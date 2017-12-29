package com.antonfagerberg

import org.scalatest.{FlatSpec, Matchers}

class Day12Test extends FlatSpec with Matchers {
  "Day 12 part 1" should "parse input" in {
    Day12.parse(
      """0 <-> 2
        |1 <-> 1
        |2 <-> 0, 3, 4
        |3 <-> 2, 4
        |4 <-> 2, 3, 6
        |5 <-> 6
        |6 <-> 4, 5""".stripMargin.split('\n').toList
    ) shouldBe Set(
      Set(0, 2),
      Set(1),
      Set(2, 0, 3, 4),
      Set(3, 2, 4),
      Set(4, 2, 3, 6),
      Set(5, 6),
      Set(6, 4, 5)
    )
  }

  it should "count connections" in {
    Day12.part1(
      """0 <-> 2
        |1 <-> 1
        |2 <-> 0, 3, 4
        |3 <-> 2, 4
        |4 <-> 2, 3, 6
        |5 <-> 6
        |6 <-> 4, 5""".stripMargin.split('\n').toList
    ) shouldBe 6
  }

  "Day 12 part 2" should "count clusters" in {
    Day12.part2(
      """0 <-> 2
        |1 <-> 1
        |2 <-> 0, 3, 4
        |3 <-> 2, 4
        |4 <-> 2, 3, 6
        |5 <-> 6
        |6 <-> 4, 5""".stripMargin.split('\n').toList
    ) shouldBe 2
  }
}
