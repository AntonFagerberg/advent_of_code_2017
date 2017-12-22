package com.antonfagerberg

import org.scalatest.{FlatSpec, Matchers}

class Day03Test extends FlatSpec with Matchers {
  "Day 03 part 1" should "return circle 0" in {
    Day03.circles(1) shouldEqual 0
  }

  it should "return circle 1" in {
    (2 to 9).foreach { i =>
      withClue(s"i = $i") {
        Day03.circles(i) shouldEqual 1
      }
    }
  }

  it should "return circle 2" in {
    (10 to 25).foreach { i =>
      withClue(s"i = $i") {
        Day03.circles(i) shouldEqual 2
      }
    }
  }

  "Day 03 part 1" should "return step 0" in {
    List(1, 2, 4, 6, 8).foreach { nr =>
      withClue(s"nr = $nr") {
        Day03.steps(Day03.circles(nr), nr) shouldEqual 0
      }
    }
  }

  it should "return step 1" in {
    List(3,5,7,9).foreach { nr =>
      withClue(s"nr = $nr") {
        Day03.steps(Day03.circles(nr), nr) shouldEqual 1
      }
    }
  }

  it should "return step 2" in {
    List(13, 17, 21, 25).foreach { nr =>
      withClue(s"nr = $nr") {
        Day03.steps(Day03.circles(nr), nr) shouldEqual 2
      }
    }
  }

  "Day 03 part 2" should "do walk" in {
    Day03.spiral(747).values.toList.sorted shouldBe
      List(1, 1, 2, 4, 5, 10, 11, 23, 25, 26, 54, 57, 59, 122, 133, 142, 147, 304, 330, 351, 362, 747, 806)

  }
}
