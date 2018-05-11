package com.antonfagerberg

object Day19 {

  type Labyrinth = Array[Array[Char]]
  type Coordinate = (Int, Int)

  def findStart(input: Labyrinth): Coordinate = {
    val Some((_, x)) =
      input
        .head
        .zipWithIndex
        .find {
          case ('|', _) => true
          case _ => false
        }

    (x, 0)
  }

  @scala.annotation.tailrec
  def walk(input: Labyrinth, coordinate: Coordinate, direction: Coordinate = (0, 1), letters: String = "", steps: Int = 0): (String, Int) = {
    val (x, y) = coordinate
    val (dx, dy) = direction
    val current = input(y)(x)

    if (current == ' ') {
      (letters, steps)
    } else {
      val (ndx, ndy) =
        if (current == '+') {
          val targets =
            input.lift(y + dx).flatMap(_.lift(x + dy)).map(_ -> (dy, dx)) ++
              input.lift(y - dx).flatMap(_.lift(x - dy)).map(_ -> (-dy, -dx))


          val List((_, (xx, yy))) =
            targets
              .filter {
                case (' ', _) => false
                case _ => true
              }

          (xx, yy)
        } else {
          direction
        }

      val newLetters =
        if (List('|', '-', ' ', '+').contains(current)) {
          letters
        } else {
          letters + current
        }

      walk(input, (x + ndx, y + ndy), (ndx, ndy), newLetters, steps + 1)
    }
  }

  def solve(input: Array[Array[Char]]): (String, Int) = {
    val coordinate = findStart(input)
    walk(input, coordinate)
  }

}

object Day19Solution extends App {
  private lazy val input = Input.getLines("day19/input").map(_.toCharArray).toArray
  println("Part 1 & 2: " + Day19.solve(input)) // 17455 too high, 17448 too low 17449 NOPE
}
