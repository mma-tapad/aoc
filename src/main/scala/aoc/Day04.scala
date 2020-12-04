package aoc

import scala.annotation.tailrec
import scala.util.matching.Regex
import scala.util.matching.Regex.Match

object Day04 extends App {

  @tailrec
  def consumeText(lines: Seq[String], strAcc: String = "", seqAcc: Seq[String] = Seq.empty): Seq[String] = {
    lines.headOption match {
      case None => seqAcc :+ strAcc
      case Some("") => consumeText(lines.tail, "", seqAcc :+ strAcc)
      case Some(str) => consumeText(lines.tail, s"$strAcc$str ", seqAcc)
    }
  }

  def solution1(passports: Seq[String]): Int = {
    val validProperties = Seq("byr:","iyr:","eyr:","hgt:","hcl:","ecl:","pid:")
    passports
      .count { config =>
        validProperties.forall(config.contains)
      }
  }

  def solution2(passports: Seq[String]): Int = {
    val validProperties: Seq[(Regex, Match => Boolean)] = Seq(
      "byr:(\\d+)\\s".r -> { m =>
        val year = m.group(1).toInt
        year >= 1920 && year <= 2002
      },
      "iyr:(\\d+)\\s".r -> { m =>
        val year = m.group(1).toInt
        year >= 2010 && year <= 2020
      },
      "eyr:(\\d+)\\s".r -> { m =>
        val year = m.group(1).toInt
        year >= 2020 && year <= 2030
      },
      "hgt:(\\d+)(cm|in)\\s".r -> { m =>
        val height = m.group(1).toInt
        m.group(2) match {
          case "cm" => height >= 150 && height <= 193
          case _    => height >= 59 && height <= 76
        }
      },
      "hcl:(#[0-9a-f]{6})\\s".r -> {_ => true},
      "ecl:(amb|blu|brn|gry|grn|hzl|oth)\\s".r -> {_ => true},
      "pid:(\\d{9})\\s".r -> {_ => true}
    )

    passports
      .count { config =>
        validProperties.forall { case (regex, validationFn) =>
          regex
            .findFirstMatchIn(config)
            .exists(validationFn)
        }
      }
  }

  val passports = consumeText(io.Source.fromResource("day04.txt").getLines.toSeq)

  println(solution1(passports))
  println(solution2(passports))
}
