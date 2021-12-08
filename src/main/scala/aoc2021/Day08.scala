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
      val key = split.head.trim

      /**
       * All digits defined can be defined by their signal length +
       * existence of either middle, botLeft, or topLeft
       *  0: length 6 && !middle
       *  1: length 2
       *  2: length 5 && botLeft
       *  3: length 5 && !botLeft && !topLeft
       *  4: length 4
       *  5: length 5 && topLeft
       *  6: length 6 && botLeft
       *  7: length 4
       *  8: length 7
       *  9: length 6 && !botLeft
       *
       *  Signals can be found using the digits 1, 4, 7, and 8
       *  topLeft: signal in 4 that is not in 1 and is used 6 times across all digits
       *  middle:  signal in 4 that is not in 1 and is used 7 times across all digits
       *  botLeft: signal that is used 4 times across all digits
       */

      val keys = key.split("\\s+").toList
      val onesDigitSignals = keys.find(_.length == 2).get
      val foursDigitSignals = keys.find(_.length == 4).get

      val (topLeft, middle) = foursDigitSignals
        .filter(c => !onesDigitSignals.contains(c))
        .partition { letter =>
          key.count(_ == letter) == 6
        }

      val botLeft = key
        .toCharArray
        .map(_.toString)
        .groupBy(identity)
        .collectFirst {
          case (k,v) if v.length == 4 => k
        }
        .get

      split.last.trim
        .split("\\s+")
        .map { output =>
          val outputChars: Set[String] = output.map(_.toString).toSet
          output.length match {
            case 2 => "1"
            case 3 => "7"
            case 4 => "4"
            case 5 =>
              outputChars.contains(topLeft) -> outputChars.contains(botLeft) match {
                case (true, _) => "5"
                case (_, true) => "2"
                case _         => "3"
              }
            case 6 =>
              outputChars.contains(middle) -> outputChars.contains(botLeft) match {
                case (false, _) => "0"
                case (_, true) => "6"
                case _ => "9"
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