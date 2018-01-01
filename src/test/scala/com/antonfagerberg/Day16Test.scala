package com.antonfagerberg

import org.scalatest.{FlatSpec, Matchers}

class Day16Test extends FlatSpec with Matchers {
  private val programs = ('a' to 'e').toArray

 "Day 16 part 1" should "do a spin" in {
    val moves = Day16.buildMoves(List("s1"), programs.length)
   moves(programs).mkString shouldBe "eabcd"
  }

  it should "do an exchange" in {
    val moves = Day16.buildMoves(List("x3/4"), programs.length)
    moves(programs).mkString shouldBe "eabdc"
  }

  it should "do a partner" in {
    val moves = Day16.buildMoves(List("pe/b"), programs.length)
    moves(programs).mkString shouldBe "baedc"
  }

  it should "do a dance" in {
    val moves =
      Day16.buildMoves(
        """s1
          |x3/4
          |pe/b""".stripMargin.split('\n').toList, programs.length
      )

    moves(programs).mkString shouldBe "ceadb"
  }
}
