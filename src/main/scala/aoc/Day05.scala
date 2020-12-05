package aoc

import scala.annotation.tailrec

object Day05 extends App {

  def shrinkCoord(min: Int, max: Int, front: Boolean): Int = {
    val distance = (max-min)/2+1
    if(front) max - distance
    else min + distance
  }

  @tailrec
  def findSeatId(seat: String, xmin: Int = 0, xmax: Int = 127, ymin: Int = 0, ymax: Int = 7): Int = {
    seat.headOption match {
      case None =>
        xmin * 8 + ymin
      case Some('F') =>
        findSeatId(seat.tail, xmin, shrinkCoord(xmin, xmax, true), ymin, ymax)
      case Some('B') =>
        findSeatId(seat.tail, shrinkCoord(xmin, xmax, false), xmax, ymin, ymax)
      case Some('L') =>
        findSeatId(seat.tail, xmin, xmax, ymin, shrinkCoord(ymin, ymax, true))
      case Some('R') =>
        findSeatId(seat.tail, xmin, xmax, shrinkCoord(ymin, ymax, false), ymax)
    }
  }

  def solution1(seats: Seq[String]): Int = {
    seats.map(findSeatId(_)).max
  }

  def solution2(seats: Seq[String]): Int = {
    val availableSeats = (8 to 881).toSet
    val takenSeats = seats
      .map(findSeatId(_))
      .toSet
    val possibleSeats = availableSeats -- takenSeats

    possibleSeats
      .filter(s => takenSeats.contains(s+1) && takenSeats.contains(s-1))
      .head
  }

  val seats = io.Source.fromResource("day05.txt").getLines.toSeq

  println(solution1(seats))
  println(solution2(seats))
}
