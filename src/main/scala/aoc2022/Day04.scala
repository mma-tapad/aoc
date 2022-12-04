package aoc2022

object Day04 extends App {
  def solution1(ranges: List[(Range, Range)]): Int = {
    ranges.count { case (range1, range2) =>
      range1.contains(range2) || range2.contains(range1)
    }
  }

  def solution2(ranges: List[(Range, Range)]): Int = {
    ranges.count { case (range1, range2) =>
      range1.overlaps(range2)
    }
  }

  val ranges = io.Source.fromResource("2022/day04.txt")
    .getLines
    .map {
      case s"$min1-$max1,$min2-$max2" =>
        Range(min1.toInt, max1.toInt) -> Range(min2.toInt, max2.toInt)
    }
    .toList

  println(solution1(ranges))
  println(solution2(ranges))
}

case class Range(min: Int, max: Int) {
  def contains(other: Range): Boolean = {
    min <= other.min && max >= other.max
  }

  def contains(num: Int): Boolean = {
    min <= num && max >= num
  }

  def overlaps(other: Range): Boolean = {
    this.contains(other.min) ||
      this.contains(other.max) ||
      other.contains(this)
  }
}