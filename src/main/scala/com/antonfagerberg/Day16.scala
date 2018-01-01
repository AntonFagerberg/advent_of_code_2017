package com.antonfagerberg

import scala.annotation.tailrec

object Day16 {
  def buildMoves(moves: List[String], size: Int): Array[Char] => Array[Char] = {
    val tempArray = Array.fill(size)('x')
    var tempIndex1 = -1
    var tempIndex2 = -1
    var tempChar = 'x'

    moves.map { move =>
      move.head match {
        case 's' =>
          val offset = move.tail.toInt
          (program: Array[Char]) => {
            Array.copy(program, 0, tempArray, 0, size)
            Array.copy(tempArray, 0, program, offset, size - offset)
            Array.copy(tempArray, size - offset, program, 0, offset)
            program
          }

        case 'x' =>
          val Array(index1, index2) = move.tail.split('/').map(_.toInt)
          (program: Array[Char]) => {
            tempChar = program(index1)
            program(index1) = program(index2)
            program(index2) = tempChar
            program
          }

        case 'p' =>
          val Array(program1, program2) = move.tail.split('/').map(_.head)
          (program: Array[Char]) => {
            tempIndex1 = program.indexOf(program1)
            tempIndex2 = program.indexOf(program2)
            tempChar = program(tempIndex1)
            program(tempIndex1) = program(tempIndex2)
            program(tempIndex2) = tempChar
            program
          }

      }
    }
    .reduce(_.andThen(_))
  }

  val part1: List[String] => String = input => {
    val programs = ('a' to 'p').toArray
    val moves = buildMoves(input, programs.length)
    moves(programs).mkString
  }

  val part2: List[String] => String = input => {
    val programs = ('a' to 'p').toArray
    val moves = buildMoves(input, programs.length)

    @tailrec
    def longDance(mutation: Array[Char], initialPrograms: Array[Char], stepsLeft: Int): String = {
      if (stepsLeft == 0) {
        mutation.mkString
      } else if (mutation.sameElements(initialPrograms)) {
        longDance(mutation, Array.empty, 1e9.toInt % (1e9.toInt - stepsLeft))
      } else {
        longDance(moves(mutation), initialPrograms, stepsLeft - 1)
      }
    }

    longDance(moves(programs.clone()), programs, 1e9.toInt - 1)
  }
}

object Day16Solution extends App {
  private lazy val input = Input.getLines("day16/input").mkString.split(',').toList
  println("Part 1: " + Day16.part1(input))
  println("Part 2: " + Day16.part2(input)) // Set -Xss16M
}