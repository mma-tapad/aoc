package aoc2021

import scala.annotation.tailrec

object Day10 extends App {
  type Score = Int
  val pairMap: Map[Char, Char] = Map(
    ')' -> '(',
    ']' -> '[',
    '}' -> '{',
    '>' -> '<'
  )

  def solution1(lines: List[String]): Score = {
    @tailrec
    def findIllegalChar(charOpt: Option[Char], accOpenings: List[Char])(line: List[Char]): Option[Char] = {
      charOpt match {
        case None => None
        case Some(char) =>
          pairMap.get(char) match {
            case None => findIllegalChar(line.headOption, char :: accOpenings)(line.drop(1))
            case Some(opening) if opening == accOpenings.head =>
              findIllegalChar(line.headOption, accOpenings.drop(1))(line.drop(1))
            case _ => Some(char)
          }
      }
    }

    val scoreMap = Map(
      ')' -> 3,
      ']' -> 57,
      '}' -> 1197,
      '>' -> 25137
    )

    lines
      .flatMap { line =>
        findIllegalChar(line.headOption, List.empty[Char])(line.drop(1).toList)
      }
      .map(scoreMap(_))
      .sum
    }

  def solution2(lines: List[String]): Long = {
    @tailrec
    def iterateLine(charOpt: Option[Char], accOpenings: List[Char])(line: List[Char]): List[Char] = {
      charOpt match {
        case None       => accOpenings
        case Some(char) =>
          pairMap.get(char) match {
            case None               => iterateLine(line.headOption, char :: accOpenings)(line.drop(1))
            case Some(opening) if opening == accOpenings.head =>
                iterateLine(line.headOption, accOpenings.drop(1))(line.drop(1))
            case _ => Nil
          }
      }
    }

    val scoreMap = Map(
      ('(', 1),
      ('[', 2),
      ('{', 3),
      ('<', 4)
    )

    val scores = lines
      .map { line =>
        iterateLine(line.headOption, List.empty[Char])(line.tail.toList)
          .foldLeft(0L) { case (totalScore, opening) =>
            (totalScore * 5) + scoreMap(opening)
          }
      }
      .filterNot(_ == 0)
      .sorted

    scores.drop(scores.size/2).head

  }

  val lines = io.Source.fromResource("2021/day10.txt").getLines.toList

  println(solution1(lines))
  println(solution2(lines))
}