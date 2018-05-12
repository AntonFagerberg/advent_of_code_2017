package com.antonfagerberg

import org.scalatest.{FlatSpec, Matchers}

class Day18Test extends FlatSpec with Matchers {

  "Day 18 part 1" should "receive sound" in {
    val program =
      """set a 1
        |add a 2
        |mul a a
        |mod a 5
        |snd a
        |set a 0
        |rcv a
        |jgz a -1
        |set a 1
        |jgz a -2"""
        .stripMargin
        .split('\n')

    Day18.runPart1(program, Map.empty, 0) shouldBe 4
  }

  "Day 18 part 2" should "execute two programs" in {
    val program =
      """snd 1
        |snd 2
        |snd p
        |rcv a
        |rcv b
        |rcv c
        |rcv d"""
        .stripMargin
        .split('\n')

    Day18.runPart2(program, (Map('p' -> 0), Map('p' -> 1)), (0, 0), (List.empty, List.empty), true, (false, false), (List.empty, List.empty)) shouldBe (List(1, 2, 0), List(1, 2, 1))
  }

}
