package com.antonfagerberg

import org.scalatest.{FlatSpec, Matchers}

class Day02Test extends FlatSpec with Matchers {
  "Day 02 part 1" should "checksum" in {
    val input =
      """|5 1 9 5
         |7 5 3
         |2 4 6 8
      """.stripMargin


    (Day02.parse _).andThen(Day02.checksum1)(input.split('\n').toList) shouldEqual 18
  }

  "Day 02 part 2" should "checksum" in {
    val input =
      """|5 9 2 8
         |9 4 7 3
         |3 8 6 5
      """.stripMargin


    (Day02.parse _).andThen(Day02.checksum2)(input.split('\n').toList) shouldEqual 9
  }
}
