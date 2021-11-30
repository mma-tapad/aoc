package aoc2020

object Day13 extends App {
  def solution1(currTime: Int, buses: Seq[String]): Int = {
    buses
      .filter(_ != "x")
      .map(_.toInt)
      .map { bus =>
        if(currTime % bus == 0) 0 -> 0
        else {
          val time = (bus * ((currTime / bus) + 1)) - currTime
          time -> bus * time
        }
      }
      .minBy(_._1)
      ._2
  }

  def solution2(schedule: Seq[String]): Double = {
    def longStream(init: Long, jump: Long): LazyList[Long] = {
      def loop(v: Long): LazyList[Long] = v #:: loop(v + jump)

      loop(init)
    }
    def findTime(buses: List[BusNumber]): Long = {
      buses
        .tail
        .foldLeft((buses.head.bus, buses.head.bus)) { case ((time, increment), nextBus) =>
          // first time where the two buses come in the correct order.
          val baseTime = longStream(time, increment).find { x =>
            (x + nextBus.index) % nextBus.bus == 0
          }.get
          // after that, maintain the cadence by finding the time that repeats the bus schedule.
          val newIncrement = increment * nextBus.bus
          (baseTime, newIncrement)
        }._1
    }

    val buses = schedule
      .zipWithIndex
      .filter(_._1 != "x")
      .map { case (busStr, index) =>
        BusNumber(busStr.toInt, index)
      }
      .toList

    findTime(buses)
  }

  val (currTime, buses) = {
    val lines = io.Source.fromResource("2020/day13.txt").getLines.toSeq

    lines.head.toInt -> lines.last.split(",").toSeq
  }

  val testBuses = "7,13,x,x,59,x,31,19".split(",").toSeq

  println(solution1(currTime, buses))
  println(solution2(buses))
}

case class BusNumber(bus: Long, index: Int)
