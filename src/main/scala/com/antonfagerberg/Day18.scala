package com.antonfagerberg

object Day18 {

  def runPart1(input: Array[String], registers: Map[Char, Long], offset: Int): Long = {
    input(offset).split(' ') match {
      case Array("snd", x) =>
        runPart1(input, registers + ('?' -> lookupValue(x, registers)), offset + 1)

      case Array("set", x, y) =>
        runPart1(input, registers + (x.head -> lookupValue(y, registers)), offset + 1)

      case Array("add", x, y) =>
        runPart1(input, registers + (x.head -> (registers.getOrElse(x.head, 0l) + lookupValue(y, registers))), offset + 1)

      case Array("mul", x, y) =>
        runPart1(input, registers + (x.head -> (registers.getOrElse(x.head, 0l) * lookupValue(y, registers))), offset + 1)

      case Array("mod", x, y) =>
        runPart1(input, registers + (x.head -> (registers.getOrElse(x.head, 0l) % lookupValue(y, registers))), offset + 1)

      case Array("rcv", x) if lookupValue(x, registers) != 0 =>
        lookupValue("?", registers)

      case Array("jgz", x, y) if lookupValue(x, registers) > 0 =>
        runPart1(input, registers, offset + lookupValue(y, registers).toInt)

      case _ =>
        runPart1(input, registers, offset + 1)
    }

  }

  private def get[T](tuple2: (T, T), left: Boolean): T = {
    val (t1, t2) = tuple2

    if (left) t1
    else t2
  }

  private def set[T](tuple2: (T, T), value: T, left: Boolean): (T, T) = {
    val (t1, t2) = tuple2

    if (left) (value, t2)
    else (t1, value)
  }

  @scala.annotation.tailrec
  def runPart2(input: Array[String], registers: (Map[Char, Long], Map[Char, Long]), offsets: (Int, Int), queues: (List[Long], List[Long]), left: Boolean, blocked: (Boolean, Boolean), queueHistory: (List[Long], List[Long])): (List[Long], List[Long]) = {
    if (blocked == (true, true)) {
      queueHistory
    } else {
      val register = get(registers, left)
      def incOffset: (Int, Int) = set(offsets, get(offsets, left) + 1, left)

      input(get(offsets, left)).split(' ') match {
        case Array("snd", x) =>
          val updatedQueues = set(queues, get(queues, left) :+ lookupValue(x, register), left)
          val updatedQueueHistory = set(queueHistory, get(queueHistory, left) :+ lookupValue(x, register), left)
          runPart2(input, registers, incOffset, updatedQueues, left, (false, false), updatedQueueHistory)

        case Array("set", x, y) =>
          val updatedRegister = register + (x.head -> lookupValue(y, register))
          runPart2(input, set(registers, updatedRegister, left), incOffset, queues, left, (false, false), queueHistory)

        case Array("add", x, y) =>
          val updatedRegister = register + (x.head -> (register.getOrElse(x.head, 0l) + lookupValue(y, register)))
          runPart2(input, set(registers, updatedRegister, left), incOffset, queues, left, (false, false), queueHistory)

        case Array("mul", x, y) =>
          val updatedRegister = register + (x.head -> (register.getOrElse(x.head, 0l) * lookupValue(y, register)))
          runPart2(input, set(registers, updatedRegister, left), incOffset, queues, left, (false, false), queueHistory)

        case Array("mod", x, y) =>
          val updatedRegister = register + (x.head -> (register.getOrElse(x.head, 0l) % lookupValue(y, register)))
          runPart2(input, set(registers, updatedRegister, left), incOffset, queues, left, (false, false), queueHistory)

        case Array("rcv", x) =>
          val queue = get(queues, !left)

          if (queue.nonEmpty) {
            val updatedRegister = register + (x.head -> queue.head)
            runPart2(input, set(registers, updatedRegister, left), incOffset, set(queues, queue.tail, !left), left, blocked, queueHistory)
          } else {
            runPart2(input, registers, offsets, queues, !left, set(blocked, true, left), queueHistory)
          }

        case Array("jgz", x, y) =>
          if (lookupValue(x, register) > 0) {
            runPart2(input, registers, set(offsets, get(offsets, left) + lookupValue(y, register).toInt, left), queues, left, blocked, queueHistory)
          } else {
            runPart2(input, registers, incOffset, queues, left, (false, false), queueHistory)
          }
      }
    }

  }

  private def lookupValue(value: String, registers: Map[Char, Long]): Long = {
    try {
      value.toLong
    } catch {
      case _: NumberFormatException => registers.getOrElse(value.head, 0)
    }
  }

  val part1: Array[String] => Long = runPart1(_, Map.empty, 0)
  val part2: Array[String] => Int = { input =>
    val (_, queueHistory) = runPart2(input, (Map('p' -> 0), Map('p' -> 1)), (0, 0), (List.empty, List.empty), true, (false, false), (List.empty, List.empty))
    queueHistory.length
  }

}

object Day18Solution extends App {
  private lazy val input = Input.getLines("day18/input").toArray
  println("Part 1: " + Day18.part1(input))
  println("Part 2: " + Day18.part2(input))
}
