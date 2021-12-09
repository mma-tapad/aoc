package aoc2021

import aoc2021.Utils.Point

object Day05 extends App {
  def countPoints(lineSeq: Seq[Line]): Map[Point, Int] = {
    lineSeq.foldLeft(Map.empty[Point, Int]) { case (countMap, straightLine) =>
      val newCounts = straightLine
        .explodePoints
        .map { point =>
          point -> countMap.get(point).map(_ + 1).getOrElse(1)
        }
      countMap ++ newCounts
    }
  }

  def solution1(lines: Seq[Line]): Int = {
    val straights = lines.filter(_.isStraight)
    val pointCounts = countPoints(straights)

    pointCounts.values.count(_ >= 2)
  }

  def solution2(lines: Seq[Line]): Int = {
    val pointCounts = countPoints(lines)

    pointCounts.values.count(_ >= 2)
  }

  val textLines = io.Source.fromResource("2021/day05.txt").getLines
  val lines: Seq[Line] = textLines.map {
    case s"$x1,$y1 -> $x2,$y2" => Line(Point(x1.toInt,y1.toInt), Point(x2.toInt, y2.toInt))
  }.toList

  println(solution1(lines))
  println(solution2(lines))
}

case class Line(start: Point, end: Point) {
  def isStraight: Boolean = start.x == end.x || start.y == end.y

  def explodePoints: Seq[Point] = {
    def findValidIndices(startIndex: Int, endIndex: Int): Seq[Int] = {
      endIndex - startIndex match {
        case 0 => Seq(startIndex)
        // if dist is negative, go left by -1
        // if dist is positive, go right by 1
        case dist => (startIndex to startIndex + dist) by dist/Math.abs(dist)
      }
    }

    val validXs: Seq[Int] = findValidIndices(start.x, end.x)
    val validYs: Seq[Int] = findValidIndices(start.y, end.y)

    validXs
      .zipAll(validYs, validXs.head, validYs.head)
      .map { case (x, y) => Point(x,y) }
  }
}
