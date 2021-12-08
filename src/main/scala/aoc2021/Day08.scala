package aoc2021

object Day08 extends App {
  def solution1(lines: List[String]) = {
    val uniqueDigitLengths = Set(2, 3, 4, 7)

    lines
      .map { case s"$_ | $pattern" =>
        pattern
          .split("\\s+")
          .count(p => uniqueDigitLengths.contains(p.length))
      }
      .sum
  }

  def solution2(lines: List[String]) = {
    lines.map { line =>
      val split = line.split("\\|")

      /**
       * All digits defined can be defined by their signal length &&
       * their intersection with digits 1, 4, 7, or 8
       *  0: length 6 && intersection with 4 is three signals && contains 1's signals
       *  1: length 2
       *  2: length 5 && intersection with 4 is two signals
       *  3: length 5 && contains 1's signals
       *  4: length 4
       *  5: length 5 && intersection with 4 is three signals
       *  6: length 6 && intersection with 1 is one signal
       *  7: length 4
       *  8: length 7
       *  9: length 6 && intersection with 4 is four signals
       */

      val keys = split.head.trim.split("\\s+").toList
      val onesDigitSignals: Set[Char] = keys
        .filter(_.length == 2)
        .flatMap(_.toList)
        .toSet
      val foursDigitSignals = keys
        .filter(_.length == 4)
        .flatMap(_.toList)
        .toSet

      split.last.trim
        .split("\\s+")
        .map { output =>
          val outputChars: Set[Char] = output.toSet
          output.length match {
            case 2 => "1"
            case 3 => "7"
            case 4 => "4"
            case 5 =>
              outputChars.intersect(onesDigitSignals).size -> outputChars.intersect(foursDigitSignals).size match {
                case (_, 2) => "2"
                case (2, _) => "3"
                case _      => "5"
              }
            case 6 =>
              outputChars.intersect(onesDigitSignals).size -> outputChars.intersect(foursDigitSignals).size match {
                case (2, 3) => "0"
                case (1, _) => "6"
                case _      => "9"
              }
            case 7 => "8"
          }
        }
        .reduce(_ + _)
    }
      .map(_.toInt)
      .sum
  }

  val lines = io.Source.fromResource("2021/day08.txt").getLines.toList

  println(solution1(lines))
  println(solution2(lines))
}