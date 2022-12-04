package aoc2022

object Day03 extends App {
  def toPriority(c: Char): Int = {
    if(c.isLower) c.toInt - 96
    else          c.toInt - 38
  }

  def solution1(rucksacks: List[String]): Int = {
    rucksacks.map { sack =>
      val (first, second) = sack.splitAt(sack.length/2)

      toPriority(first.intersect(second).head)
    }
    .sum
  }

  def solution2(rucksacks: List[String]): Int = {
    rucksacks
      .grouped(3)
      .map {
        case first :: second :: third :: Nil => first.intersect(second).intersect(third).head
      }
      .map(toPriority)
      .sum
  }

  val rucksacks = io.Source.fromResource("2022/day03.txt").getLines.toList

  println(solution1(rucksacks))
  println(solution2(rucksacks))
}
