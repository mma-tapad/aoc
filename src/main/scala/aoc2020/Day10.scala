package aoc2020

object Day10 extends App {

  def solution1(jolts: Array[Int]): Int = {
    val jumps = jolts
      .sliding(2, 1)
      .map { window =>
        window(1) - window(0)
      }
      .toSeq

    jumps.count(_ == 1) * jumps.count(_ == 3)
  }

  def solution2(jolts: Array[Int], maxJump: Int = 3): Double = {
    val necessaryIndices = (1 until jolts.length - 1)
      .filter { index =>
        val curr = jolts(index)
        curr - jolts(index-1) == maxJump || jolts(index+1) - curr == maxJump
      }

    (0 +: necessaryIndices :+ jolts.length-1)
      .sliding(2,1)
      .map { window =>
        val distance = (window(1) - window(0)) - 1
        Math.max(1, Math.pow(2, distance) - Math.max(0, distance - (maxJump - 1)))
      }
      .product
  }

  val jolts = io.Source.fromResource("2020/day10.txt").getLines.toArray.map(_.toInt).sorted
  val realJolts = 0 +: jolts :+ jolts.last + 3

  println(solution1(realJolts))
  println(solution2(realJolts))
}
