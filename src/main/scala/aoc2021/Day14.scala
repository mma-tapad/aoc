package aoc2021

import scala.annotation.tailrec

object Day14 extends App {
  type Token = String

  def parseInstructions(lines: List[String]): (String, Map[String, Char]) = {
    val (templates, insertRules) = lines.partition(!_.contains("->"))

    templates.head ->
      insertRules.map {
        case s"$start -> $end" => start -> end.head
      }.toMap
  }

  @tailrec
  // Given some tokens, calculate what tokens they will produce.
  def iterateTokenCount(tokenCounts: Map[Token, Long], insertRules: Map[String, Char])(numTimes: Int): Map[Token, Long] = {
    numTimes match {
      case 0     => tokenCounts
      case times =>
        val newCounts: Map[Token, Long] = tokenCounts
          .foldLeft(Map.empty[Token, Long].withDefaultValue(0L)) { case (acc, (token, count)) =>
            val (newToken1, newToken2) = {
              val insert = insertRules(token)
              (token.take(1) + insert) -> (insert + token.drop(1))
            }

            acc
              .updated(newToken1, acc(newToken1) + count)
              .updated(newToken2, acc(newToken2) + count)
          }

        iterateTokenCount(newCounts, insertRules)(times-1)
    }
  }

  def helper(lines: List[String], numTimes: Int): Long = {
    val (template, insertRules) = parseInstructions(lines)
    val tokenCount = template
      .sliding(2)
      .toList
      .groupBy(identity)
      .view
      .mapValues(_.length.toLong)
      .toMap
    val tokenCounts: Map[Token, Long] = iterateTokenCount(tokenCount, insertRules)(numTimes)

    val charCount = {
      // Given a token "FK", it produces two tokens: "FO" and "OK".
      // To not double count the newly inserted 'O', we only count the first char of every produced token.
      // So we need to +1 to the count of the last char of the original template,
      // since it was dropped and not made up by the next token.
      val withoutStraggler = tokenCounts.foldLeft(Map.empty[Char, Long].withDefaultValue(0L)) { case (acc, (token, count)) =>
        acc.updated(token.head, acc(token.head) + count)
      }
      withoutStraggler.updated(template.last, withoutStraggler(template.last)+1L)
    }
    .values

    charCount.max - charCount.min
  }

  def solution1(lines: List[String]): Long = {
    helper(lines, 10)
  }

  def solution2(lines: List[String]): Long = {
    helper(lines, 40)
  }

  val lines = io.Source.fromResource("2021/day14.txt").getLines.filterNot(_.isEmpty).toList

  println(solution1(lines))
  println(solution2(lines))
}