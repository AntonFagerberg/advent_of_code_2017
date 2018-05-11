package com.antonfagerberg

import scala.annotation.tailrec

class HackBuffer[T](val value: T) {
  var next: HackBuffer[T] = this
  var previous: HackBuffer[T] = this

  def rotate(steps: Int): HackBuffer[T] = {
    @scala.annotation.tailrec
    def rotatePrivate(steps: Int, result: HackBuffer[T]): HackBuffer[T] = {
      if (steps == 0) {
        result
      } else if (steps < 0) {
        rotatePrivate(steps + 1, result.previous)
      } else {
        rotatePrivate(steps - 1, result.next)
      }
    }

    rotatePrivate(steps, this)
  }

  def find(elem: T): HackBuffer[T] = {
    @scala.annotation.tailrec
    def findPrivate(target: HackBuffer[T] = this): HackBuffer[T] = {
      if (target == this) {
        throw new NoSuchElementException(s"Element $elem does not exist in buffer")
      } else if (target.value == elem) {
        target
      } else {
        findPrivate(target.next)
      }
    }

    if (value == elem) {
      this
    } else {
      findPrivate(next)
    }
  }

  def add(elem: T): HackBuffer[T] = {
    val newBuffer = new HackBuffer(elem)
    newBuffer.next = next
    next = newBuffer
    next.previous = this
    next.next.previous = next
    newBuffer
  }

  def takeToList(size: Int): List[T] = {
    var elem = this
    List.fill(size) {
      val res = elem.value
      elem = elem.next
      res
    }
  }
}

object Day17 {
  @tailrec
  def buildBuffer(maxValue: Int, jumps: Int, buffer: HackBuffer[Int] = new HackBuffer(0), count: Int = 1): HackBuffer[Int] = {
    if (count > maxValue) {
      buffer
    } else {
      val moduloJumps = jumps % count

      val rotatedBuffer =
        if (moduloJumps > count / 2 + 1) {
          buffer.rotate(moduloJumps - count)
        } else {
          buffer.rotate(moduloJumps)
        }

      val nextBuffer = rotatedBuffer.add(count)

      buildBuffer(maxValue, jumps, nextBuffer, count + 1)
    }
  }


  def part1(input: Int): Int = buildBuffer(2017, input).next.value

  def part2(input: Int): Int = buildBuffer(50000000, input).find(0).next.value
}

object Day17Solution extends App {
  private lazy val input = Input.getLines("day17/input").mkString.toInt
  println("Part 1: " + Day17.part1(input))
  // Slow (~15 min on my laptop), did not try to optimize (can probably be optimized due to target = 0)
  println("Part 2: " + Day17.part2(input))
}
