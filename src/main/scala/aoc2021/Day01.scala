package aoc2021

object Day01 extends App {

  def solution1(nums: Array[Int]): Int = {
    (1 until nums.length).count { i =>
      nums(i - 1) < nums(i)
    }
  }

  def solution2(nums: Array[Int]): Int = {
    val windows = nums.sliding(3).toArray

    (1 until windows.length).count { i =>
      windows(i-1).sum < windows(i).sum
    }
  }

  val nums = io.Source.fromResource("2021/day01.txt").getLines.map(_.toInt).toArray

  println(solution1(nums))
  println(solution2(nums))
}
