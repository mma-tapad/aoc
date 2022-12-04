package aoc2022

object Day03 extends App {
  // 'a-z' => 1-26, 'A-Z' => 27-52
  def toPriority(c: Char): Int = {
    if(c.isLower) c.toInt - 96
    else          c.toInt - 38
  }

  // Sum the priority value of the char repeated in both halves of every line.
  def solution1(rucksacks: List[String]): Int = {
    rucksacks.map { sack =>
      val (first, second) = sack.splitAt(sack.length/2)

      toPriority(first.intersect(second).head)
    }
    .sum
  }

  // Given groups of 3 lines, sum the value of the char found in all 3 lines of a group.
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
