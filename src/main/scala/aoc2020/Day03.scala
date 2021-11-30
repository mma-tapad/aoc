package aoc2020

object Day03 extends App {

  def solution1(lines: Seq[String], rightJump: Int = 3, downJump: Int = 1) = {
    lines
      .zipWithIndex
      .count { case (line, index) =>
        index % downJump == 0 &&
          line.charAt((index * rightJump) % line.length) == '#'
      }
  }

  def solution2(lines: Seq[String]): BigInt = {
    BigInt(solution1(lines, 1, 1)) *
      solution1(lines, 3, 1) *
      solution1(lines, 5, 1) *
      solution1(lines, 7, 1) *
      solution1(lines, 1, 2)
  }

  val map = io.Source.fromResource("2020/day03.txt").getLines.toSeq

  println(solution1(map))
  println(solution2(map))
}
