package aoc

object Day01 extends App {

  def solution1(nums: Set[Int]): Option[Int] = {
    nums
      .find(n => nums.contains(2020 - n))
      .map(n => n * (2020 - n))
  }

  def solution2(nums: Set[Int]): Option[Int] = {
    nums
      .flatMap { num =>
        val sumNeeded = 2020 - num
        nums
          .find(n => nums.contains(sumNeeded - n))
          .map(n => (num, n, sumNeeded - n))
      }
      .headOption
      .map { case (a,b,c) =>
        a * b * c
      }
  }

  val nums = io.Source.fromResource("day01.txt").getLines.map(_.toInt).toSet

  println(solution1(nums))
  println(solution2(nums))
}
