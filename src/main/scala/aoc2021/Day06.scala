package aoc2021

object Day06 extends App {
  type Age = Int
  type Count = Long

  def countdown(countByAge: Map[Age, Count]): Map[Age, Count] = {
    countByAge.flatMap {
      case (0, cnt) => Seq(6 -> (cnt + countByAge.getOrElse(7, 0L)), 8 -> cnt)
      case (7, cnt) => Seq(6 -> (cnt + countByAge.getOrElse(0, 0L)))
      case (age, cnt) => Seq((age-1, cnt))
    }
  }

  def liveLife(fish: List[Int], days: Int): Count = {
    val countByAge: Map[Age, Count] = fish
      .groupBy(identity)
      .view
      .mapValues(_.size.toLong)
      .toMap

    (0 until days)
      .foldLeft(countByAge) { case (currCounts, _) =>
        countdown(currCounts)
      }
      .values
      .sum
  }

  def solution1(nums: List[Int]) = {
    liveLife(nums, 80)
  }

  def solution2(nums: List[Int]) = {
    liveLife(nums, 256)
  }

  val nums = io.Source.fromResource("2021/day06.txt").getLines.next().split(",").map(_.toInt).toList

  println(solution1(nums))
  println(solution2(nums))
}