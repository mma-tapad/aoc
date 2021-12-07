package aoc2021

object Day07 extends App {
  def findCheapestPosition(originalPositions: List[Int], candidatePositions: List[Int], fuelRate: (Int, Int) => Long): Long = {
    candidatePositions.map { cPos =>
      originalPositions.map(oPos => fuelRate(cPos, oPos)).sum -> cPos
    }.minBy(_._1)._1
  }

  def solution1(positions: List[Int]): Long = {
    findCheapestPosition(
      positions,
      positions.distinct,
      (a,b) => Math.abs(a-b)
    )
  }

  def solution2(positions: List[Int]): Long = {
    findCheapestPosition(
      positions,
      (positions.min to positions.max).toList,
      (a,b) => {
        val distance = Math.abs(a-b)
        (1 to distance).sum
      }
    )
  }

  val positions = io.Source.fromResource("2021/day07.txt").getLines.next.split(",").toList.map(_.toInt)

  println(solution1(positions))
  println(solution2(positions))
}
