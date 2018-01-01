package com.antonfagerberg

import org.scalatest.{FlatSpec, Matchers}

class Day15Test extends FlatSpec with Matchers {
 "Day 15 part 1" should "create generator A" in {
   val generatorA = Day15.generator(65, 16807)
   generatorA.take(5).toList shouldBe List(1092455, 1181022009, 245556042, 1744312007, 1352636452)
  }

  it should "create generator B" in {
    val generatorB = Day15.generator(8921, 48271)
    generatorB.take(5).toList shouldBe List(430625591, 1233683848, 1431495498, 137874439, 285222916)
  }

  it should "find match counts" in {
    val generatorA = Day15.generator(65, 16807)
    val generatorB = Day15.generator(8921, 48271)

    Day15.matchCount(generatorA, generatorB, 40000000) shouldBe 588
  }

  "Day 15 part 2" should "work with filtered generators" in {
    Day15.part2("65\n8921".stripMargin.split('\n').toList) shouldBe 309
  }
}
