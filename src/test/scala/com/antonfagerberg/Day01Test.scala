package com.antonfagerberg

import org.scalatest.{FlatSpec, Matchers}

class Day01Test extends FlatSpec with Matchers {
  "Day 01 part 1" should "resolve 1122 " in {
    Day01.part1("1122") shouldEqual 3
  }

  it should "resolve 1111" in {
    Day01.part1("1111") shouldEqual 4
  }

  it should "resolve 1234" in {
    Day01.part1("1234") shouldEqual 0
  }

  it should "resolve 91212129" in {
    Day01.part1("91212129") shouldEqual 9
  }

  "Day 01 part 2" should "resolve 1212 " in {
    Day01.part2("1212") shouldEqual 6
  }

  it should "resolve 1221 " in {
    Day01.part2("1221") shouldEqual 0
  }

  it should "resolve 123425 " in {
    Day01.part2("123425") shouldEqual 4
  }

  it should "resolve 123123 " in {
    Day01.part2("123123") shouldEqual 12
  }

  it should "resolve 12131415 " in {
    Day01.part2("12131415") shouldEqual 4
  }
}
