package com.antonfagerberg

object Day14 {
  def toBinary(input: String): List[Int] = {
    input
      .flatMap { char =>
        val intValue = Integer.parseInt(char.toString, 16)

        List(
          (intValue & 8) / 8,
          (intValue & 4) / 4,
          (intValue & 2) / 2,
          (intValue & 1)
        )
      }
      .toList
  }

  def part1(key: String): Int = {
    (0 to 127)
      .map(i => Day10.part2(s"$key-$i"))
      .flatMap(toBinary)
      .count(_ == 1)
  }

  def clearRegion(grid: Array[Array[Int]], x: Int, y: Int): Int = {
    if (y >= 0 && x >= 0 && y < grid.length && x < grid(y).length && grid(y)(x) == 1) {
      grid(y)(x) = 0
      clearRegion(grid, x + 1, y)
      clearRegion(grid, x - 1, y)
      clearRegion(grid, x, y + 1)
      clearRegion(grid, x, y - 1)
      1
    } else {
      0
    }
  }

  def countRegions(grid: Array[Array[Int]]): Int = {
    grid
      .zipWithIndex
      .flatMap { case (row, y) => row.indices.map(clearRegion(grid, _, y)) }
      .sum
  }

  def part2(key: String): Int = {
    val grid = (0 to 127).map(i => toBinary(Day10.part2(s"$key-$i")).toArray).toArray
    countRegions(grid)
  }

}

object Day14Solution extends App {
  private lazy val input = Input.getLines("day14/input").head
  println("Part 1: " + Day14.part1(input))
  println("Part 2: " + Day14.part2(input))
}