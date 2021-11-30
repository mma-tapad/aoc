package aoc2020

object Day02 extends App {

  def solution1(pwConfigs: Seq[String]): Int = {
    pwConfigs.count { case s"$min-$max $letter: $pw" =>
      val numLetters = pw.count(_ == letter(0))
      min.toInt <= numLetters && max.toInt >= numLetters
    }
  }

  def solution2(pwConfigs: Seq[String]): Int = {
    pwConfigs.count { case s"$pos1-$pos2 $letter: $pw" =>
      (pw(pos1.toInt-1) == letter(0)) != (pw(pos2.toInt-1) == letter(0))
    }
  }

  val pwConfigs = io.Source.fromResource("2020/day02.txt").getLines.toSeq

  println(solution1(pwConfigs))
  println(solution2(pwConfigs))
}
