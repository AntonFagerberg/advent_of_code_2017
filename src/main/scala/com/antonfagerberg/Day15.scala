package com.antonfagerberg

object Day15 {
  type Generator = Long => Long

  def generator(seed: Long, factor: Long): Iterator[Long] = {
    val f = (previous: Long) => (previous * factor) % 2147483647
    Iterator.iterate(f(seed))(f)
  }

  def matchCount(generatorA: Iterator[Long], generatorB: Iterator[Long], maxValues: Int): Int = {
    generatorA
      .zip(generatorB)
      .take(maxValues)
      .count { case (a, b) => a.toShort == b.toShort }
  }

  def parse(input: List[String]): (Long, Long) = {
    val List(a, b) = input.map(_.split(' ').last.toLong)
    (a, b)
  }

  def part1(input: List[String]): Int = {
    val (seedA, seedB) = parse(input)
    matchCount(generator(seedA, 16807), generator(seedB, 48271), 40000000)
  }

  def part2(input: List[String]): Int = {
    val (seedA, seedB) = parse(input)
    val generatorA = generator(seedA, 16807).filter(_ % 4 == 0)
    val generatorB = generator(seedB, 48271).filter(_ % 8 == 0)
    matchCount(generatorA, generatorB, 5000000)
  }
}

object Day15Solution extends App {
  private lazy val input = Input.getLines("day15/input")
  println("Part 1: " + Day15.part1(input))
  println("Part 2: " + Day15.part2(input))
}