package com.antonfagerberg

object Day08 {
  def execute(registers: Map[String, Int])(input: String): Map[String, Int] = {
    val pieces = raw"([a-z]+) (inc|dec) ([-0-9]+) if ([a-z]+) ([<>=!]+) ([-0-9]+)".r
    input match { case pieces(register, incDec, amount, conditionalRegister, equalityOperator, conditionalAmount) =>
      val comparisonFunction: (Int, Int) => Boolean =
        equalityOperator match {
          case ">" => _ > _
          case ">=" => _ >= _
          case "<" => _ < _
          case "<=" => _ <= _
          case "!=" => _ != _
          case "==" => _ == _
        }

      if (comparisonFunction(registers.getOrElse(conditionalRegister, 0), conditionalAmount.toInt)) {
        val valueUpdate =
          incDec match {
            case "inc" => amount.toInt
            case "dec" => -amount.toInt
          }

        registers + (register -> (registers.getOrElse(register, 0) + valueUpdate))
      } else {
        registers
      }
    }
  }

  def compute(input: List[String]): Map[String, Int] = {
    input.foldLeft(Map.empty[String, Int])(execute(_)(_))
  }

  val part1: List[String] => Int = compute(_).values.max
  val part2: List[String] => Int = _.scanLeft(Map.empty[String, Int])(execute(_)(_)).flatMap(_.values).max
}

object Day08Solution extends App {
  private lazy val input = Input.getLines("day08/input")

  println("Part 1: " + Day08.part1(input))
  println("Part 2: " + Day08.part2(input))
}