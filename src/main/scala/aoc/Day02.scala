package aoc

import scala.util.matching.Regex

object Day02 extends App {

  def solution1(pwConfigs: Seq[String], regex: Regex): Int = {
    pwConfigs.count { case regex(min, max, letter, pw) =>
      val numLetters = pw.count(_ == letter(0))
      min.toInt <= numLetters && max.toInt >= numLetters
    }
  }

  def solution2(pwConfigs: Seq[String], regex: Regex): Int = {
    pwConfigs.count { case regex(pos1, pos2, letter, pw) =>
      (pw(pos1.toInt-1) == letter(0)) != (pw(pos2.toInt-1) == letter(0))
    }
  }

  val pwConfigs = io.Source.fromResource("day02.txt").getLines.toSeq
  val regex = """(\d+)-(\d+) (\w): (\w+)""".r

  println(solution1(pwConfigs, regex))
  println(solution2(pwConfigs, regex))
}
