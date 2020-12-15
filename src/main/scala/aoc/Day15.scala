package aoc

object Day15 extends App {

  def solution1(startingNums: Seq[Int], numTimes: Int): Int = {
    val recencyMap: Map[Int, Recency] = startingNums
      .zipWithIndex
      .map { case (num, index) =>
        num -> Recency(index)
      }
      .toMap

    (startingNums.size until numTimes)
      .foldLeft((startingNums.last, recencyMap)) { case ((previousNum, rMap), currTurnNum) =>
        val recency = rMap(previousNum)
        val age = recency.age.getOrElse(0)
        val ageRecency = rMap.get(age).map(_.mostRecentTurn)
        age -> rMap.updated(age, Recency(currTurnNum, ageRecency))
      }
      ._1
  }

  val startingNums = Seq(2,20,0,4,1,17)

  println(solution1(startingNums, 2020))
  println(solution1(startingNums, 30000000))
}

case class Recency(mostRecentTurn: Int, nextMostRecentTurn: Option[Int] = None) {
  def age: Option[Int] = nextMostRecentTurn.map(mostRecentTurn - _)
}