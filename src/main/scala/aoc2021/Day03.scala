package aoc2021

object Day03 extends App {
  def solution1(lines: List[String]): Int = {
    val sums = 0 until lines.head.length map { i =>
      lines.map { bits =>
        bits(i).asDigit
      }.sum
    }

    val epsilonRate = {
      val str = sums.foldLeft("") { case (acc, sum) =>
        acc + (sum / (lines.size / 2)).toString
      }

      Integer.parseInt(str, 2)
    }

    val gammaRate = {
      val bitMask = (Math.pow(2, lines.head.length)-1).toInt
      ~epsilonRate & bitMask // bitwise NOT flips the bits, but needs bitwise AND to mask leading 1's back to 0.
    }

    epsilonRate * gammaRate
  }

  def solution2(lines: List[String]): Int = {
    def choosePartition(filteredLines: List[String], index: Int, chooseCommon: Boolean = true) = {
      val (ones, zeroes) = filteredLines.partition(_(index) == '1')
      val isOneMostCommon = ones.size >= zeroes.size

      if(isOneMostCommon ^ chooseCommon) zeroes
      else ones
    }

    val oxygenRate = {
      val str = (0 until lines.head.length).foldLeft(lines) { case (filteredLines, index) =>
        choosePartition(filteredLines, index)
      }.head

      Integer.parseInt(str, 2)
    }

    val co2Rate = {
      val str = (0 until lines.head.length).foldLeft(lines) {
        case (filteredLines, _) if filteredLines.size == 1 =>
          filteredLines
        case (filteredLines, index) =>
          choosePartition(filteredLines, index, chooseCommon = false)
      }.head

      Integer.parseInt(str, 2)
    }

    oxygenRate * co2Rate
  }

  val lines = io.Source.fromResource("2021/day03.txt").getLines.toList

  println(solution1(lines))
  println(solution2(lines))
}

