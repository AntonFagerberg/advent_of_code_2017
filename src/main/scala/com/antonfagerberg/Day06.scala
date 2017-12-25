package com.antonfagerberg

import scala.annotation.tailrec
import scala.collection.mutable

object Day06 {
  private def mutateBanks(banks: mutable.ArrayBuffer[Int]): Unit = {
    val (maxValue, maxIndex) =
      banks.zipWithIndex.reduce[(Int, Int)] { case ((value1, index1), (value2, index2)) =>
        if (value2 > value1) {
          (value2, index2)
        } else {
          (value1, index1)
        }
      }

    val bankSize = banks.size

    val fullCycles = maxValue / bankSize
    val additionalBumps = maxValue % bankSize


    banks(maxIndex) = 0

    banks.indices.foreach(banks(_) += fullCycles)

    (1 to additionalBumps).foreach { offset =>
      banks((maxIndex + offset) % bankSize) += 1
    }
  }

  @tailrec
  private def redistribute(banks: mutable.ArrayBuffer[Int], history: mutable.Set[List[Int]] = mutable.Set.empty, redistributionCount: Int = 0): Int = {
    mutateBanks(banks)

    val immutableBanks = banks.toList

    if (history.contains(immutableBanks)) {
      1 + redistributionCount
    } else {
      redistribute(banks, history + immutableBanks, 1 + redistributionCount)
    }
  }

  @tailrec
  private def redistributeLoop(banks: mutable.ArrayBuffer[Int], history: mutable.HashMap[List[Int], Int] = mutable.HashMap.empty, redistributionCount: Int = 0): Int = {
    mutateBanks(banks)

    val immutableBanks = banks.toList

    if (history.contains(immutableBanks)) {
      redistributionCount - history(immutableBanks)
    } else {
      redistributeLoop(banks, history += (immutableBanks -> redistributionCount), 1 + redistributionCount)
    }
  }

  val part1: Seq[Int] => Int = input => redistribute(input.to[mutable.ArrayBuffer])
  val part2: Seq[Int] => Int = input => redistributeLoop(input.to[mutable.ArrayBuffer])
}

object Day06Solution extends App {
  private lazy val input = Input.getLines("day06/input").head.split("\\s+").map(_.toInt)

  println("Part 1: " + Day06.part1(input))
  println("Part 2: " + Day06.part2(input))
}
