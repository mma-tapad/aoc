package aoc

import scala.annotation.tailrec

object Day18 extends App {
  val parenRegex = """\(([^(]+?)\)""".r

  @tailrec
  def resolveParens(block: String, replaceMap: Map[String, String] = Map.empty)(doMath: String => String): String = {
    val updatedBlock = parenRegex
      .findAllMatchIn(block)
      .foldLeft(replaceMap) { case (acc, m) =>
        acc.updated(m.group(0), doMath(m.group(1)))
      }
      .foldLeft(block) { case (acc, (key, value)) =>
        acc.replace(key, value)
      }

    if(updatedBlock.contains('(')) {
      resolveParens(updatedBlock)(doMath)
    } else {
      doMath(updatedBlock)
    }
  }

  def doArithmetic(block: String): String = {
    block
      .split(" ")
      .foldLeft(0L, {0L + _}: Long => Long) { case ((acc, longFn), term) =>
        term match {
          case "*" => (acc, acc * _)
          case "+" =>(acc, acc + _)
          case number => (longFn(number.toLong), longFn)
        }
      }
      ._1.toString
  }

  def solution1(mathLines: Seq[String]): Long = {
    mathLines.foldLeft(0L) { case (acc, line) =>
      acc + doArithmetic(resolveParens(line)(doArithmetic)).toLong
    }
  }

  def solution2(mathLines: Seq[String]): BigInt = {
    val addRegex = """(\d+) \+ (\d+)""".r

    def doAdditionPriority(block: String): String = {
      @tailrec
      def resolveAdditionHelper(block: String, replaceMap: Map[String, String] = Map.empty): String = {
        val updatedBlock = addRegex
          .findAllMatchIn(block)
          .foldLeft(replaceMap) { case (acc, m) =>
            acc.updated(m.group(0), (m.group(1).toLong + m.group(2).toLong).toString)
          }
          .toSeq
          .sortBy(_._1.length)
          .reverse
          .foldLeft(block) { case (acc, (key, value)) =>
            acc.replace(key, value)
          }

        if(updatedBlock.contains('+')) {
          resolveAdditionHelper(updatedBlock)
        } else {
          doArithmetic(updatedBlock)
        }
      }

      resolveAdditionHelper(block)
    }

    mathLines.foldLeft(0L) { case (acc, line) =>
      acc + resolveParens(line)(doAdditionPriority).toLong
    }
  }

  val mathLines = io.Source.fromResource("day18.txt").getLines.toSeq

  println(solution1(mathLines))
  println(solution2(mathLines))
}
