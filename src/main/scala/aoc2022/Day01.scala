package aoc2022

import scala.annotation.tailrec

object Day01 extends App {
  // Sum up calories on every line until we hit a blank line (case None).
  // That's when you save your sum in the res List and restart from 0.
  @tailrec
  def consume(list: List[Option[Int]], currCalories: Int = 0)(res: List[Int] = Nil): List[Int] = {
    list match {
      case Nil                    => currCalories :: res
      case None :: tail           => consume(tail)(currCalories :: res)
      case Some(calories) :: tail => consume(tail, currCalories + calories)(res)
    }
  }

  // Find the highest calorie block
  def solution1(nums: List[Option[Int]]): Int = {
    consume(nums)().max
  }

  // Find the top 3 highest calorie blocks
  def solution2(nums: List[Option[Int]]): Int = {
    consume(nums)().sorted.takeRight(3).sum
  }

  val nums = io.Source.fromResource("2022/day01.txt").getLines.map(_.toIntOption).toList

  println(solution1(nums))
  println(solution2(nums))
}
