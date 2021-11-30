package aoc2020

import scala.annotation.tailrec
import scala.util.matching.Regex
import scala.util.matching.Regex.Match

object Day04 extends App {

  def solution1(passports: Seq[String]): Int = {
    val validProperties = Seq("byr:","iyr:","eyr:","hgt:","hcl:","ecl:","pid:")
    passports.count { config =>
      validProperties.forall(config.contains)
    }
  }

  def solution2(passports: Seq[String]): Int = {
    def validateYear(m: Match, min: Int, max: Int): Boolean = {
      val year = m.group(1).toInt
      year >= min && year <= max
    }

    def validateHeight(m: Match): Boolean = {
      val height = m.group(1).toInt
      m.group(2) match {
        case "cm" => height >= 150 && height <= 193
        case _    => height >= 59 && height <= 76
      }
    }

    val validProperties: Seq[(Regex, Match => Boolean)] = Seq(
      "byr:(\\d+)\\s".r                        -> {validateYear(_, 1920, 2002)},
      "iyr:(\\d+)\\s".r                        -> {validateYear(_, 2010, 2020)},
      "eyr:(\\d+)\\s".r                        -> {validateYear(_, 2020, 2030)},
      "hgt:(\\d+)(cm|in)\\s".r                 -> validateHeight,
      "hcl:(#[0-9a-f]{6})\\s".r                -> {_ => true},
      "ecl:(amb|blu|brn|gry|grn|hzl|oth)\\s".r -> {_ => true},
      "pid:(\\d{9})\\s".r                      -> {_ => true}
    )

    passports.count { config =>
      validProperties.forall { case (regex, validationFn) =>
        regex
          .findFirstMatchIn(config)
          .exists(validationFn)
      }
    }
  }

  val passports = Utils.consumeText(io.Source.fromResource("2020/day04.txt").getLines.toSeq)

  println(solution1(passports))
  println(solution2(passports))
}
